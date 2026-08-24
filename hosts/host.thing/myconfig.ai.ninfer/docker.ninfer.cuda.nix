# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# CUDA (NVIDIA RTX 5090) NInfer Podman configuration.
#
# Modelled on ../myconfig.ai.vllm/docker.vllm.cuda.nix's
# `vllmQwen38_27B_NVFP4` variant, but using NInfer
# (https://github.com/Neroued/ninfer) instead of vLLM. Serves
# Qwen3.8-27B NVFP4 (https://huggingface.co/neroued/Qwen3.8-27B-nvfp4-NInfer)
# on the same host port (22545) as the CUDA vLLM variants, so llama-swap
# can swap the engine in and out on the same GPU.

{
  config,
  pkgs,
  lib,
  ...
}:

let
  common = import ./common.nix { inherit pkgs; };
  mkNinfer = common.mkNinferDockerized;

  port = 22545;
  # Public OpenAI-facing model id (the engine accepts exactly one).
  # Uses the generic "localhost:<port>" name so clients that address
  # the container port directly work without knowing the internal
  # name: the { port = 22545; } entry in myconfig.ai.localModels
  # (host.thing/default.nix) becomes the "localhost:22545" model on
  # http://localhost:22545/v1 in pi-coding-agent, mirroring the vLLM
  # variants that serve "localhost:$HOST_PORT" alongside their names.
  # Also the model name the aichat client below uses; overridable at
  # launch via the MODEL_ID env var.
  modelId = "localhost:${toString port}";

  # --- Qwen3.8-27B NVFP4 (NInfer artifact) ---
  # The model subvolume is mounted writable at /home/mhuber/models and
  # read-only at /models: downloads write through the former, serving
  # (checks, checksum, bind mount) reads through the latter.
  ninferQwen38_27B_NVFP4 = mkNinfer {
    modelDownloadDir = "/home/mhuber/models/neroued-Qwen3.8-27B-nvfp4-NInfer";
    modelHostDir = "/models/neroued-Qwen3.8-27B-nvfp4-NInfer";
    modelFilename = "qwen3_8_27b_nvfp4.ninfer";
    # Published SHA-256 (model card / README table / SHA256SUMS).
    modelSha256 = "bb3360522a06e136e0367f5703414d26272b7285c8a6ab6194135c17dbd81b32";
    modelHfRepo = "neroued/Qwen3.8-27B-nvfp4-NInfer";
    inherit modelId;
    servedModelName = "Qwen3.8-27B-NVFP4";
    containerName = "ninfer-dockerized-Qwen3.8-27B-NVFP4";
    inherit port;
    # Half of the published saturated RTX 5090 context (252,928, the
    # factory default) so the INT8 KV cache has headroom on the card.
    # Overridable at launch via MAX_CONTEXT.
    maxContext = 126464;
    # Pinned engine source (master @ 2026-08-19); newer than the
    # artifact's minimum_revision (5d2c1f55, see artifact-manifest.json).
    # The image is built from this revision on first launch.
    ninferGitRef = "feaf4dd0983fdaeb2ba4c06eec6da350e644fb3a";
    # This variant is the primary (and only) NInfer model, so it claims
    # the bare "ninfer" alias. llama-swap hard-fails on duplicate
    # aliases, so none of the vLLM-variant aliases (localhost:22545,
    # vllm:*, the bare model names) are reused.
    extraConfig = {
      aliases = [ "ninfer" ];
    };
  };
in
{
  imports = [
    {
      # Self-contained defaults (consistent with the vLLM siblings);
      # the llama-swap unit's Podman and NVIDIA CDI start dependencies
      # are declared in ../myconfig.ai.vllm/default.nix so they are not
      # duplicated.
      virtualisation.podman.enable = lib.mkDefault true;
      # NVIDIA CDI specs come from the same generator that served Docker.
      hardware.nvidia-container-toolkit.enable = lib.mkDefault true;
    }
  ];

  config = {
    environment.systemPackages = [
      ninferQwen38_27B_NVFP4.ninferPkg
    ];

    services.llama-swap.settings.models = ninferQwen38_27B_NVFP4.modelConfig;

    # Declared next to the variant above (rather than in
    # host.thing/default.nix) so this pull spec isn't silently left
    # behind (and still downloaded) if the variant is ever dropped.
    myconfig.ai.pull_models.models."/home/mhuber/models" = [
      "neroued/Qwen3.8-27B-nvfp4-NInfer" # ninferQwen38_27B_NVFP4
    ];

    home-manager.sharedModules = [
      {
        programs.aichat.settings.clients = [
          {
            type = "openai-compatible";
            name = "ninfer";
            # Points straight at the container port (like the vllm
            # client), so the model name is the served public model
            # id, not a llama-swap alias.
            api_base = "http://localhost:${toString port}/v1";
            models = [
              { name = modelId; }
            ];
          }
        ];
      }
    ];
  };
}
