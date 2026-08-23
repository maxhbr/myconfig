# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# NInfer (https://github.com/Neroued/ninfer) Qwen3.8-27B NVFP4
# container launch configuration.
#
# NInfer is a from-scratch C++/CUDA inference engine for registered
# model artifacts. Unlike vLLM there is no public registry image —
# the runtime image is built once from pinned GitHub source into the
# local Podman store — and the model is a single `.ninfer` artifact
# file (~20 GiB, weights + frontend resources), not a safetensors
# directory.
#
# Container runtime: Podman (NVIDIA GPU access via CDI, same as the
# vLLM CUDA variants in this directory).
#
# Serves Qwen3.8-27B NVFP4
# (https://huggingface.co/neroued/Qwen3.8-27B-nvfp4-NInfer) on the
# same host port (22545) as the CUDA vLLM variants, so llama-swap can
# swap the engine in and out on the same GPU. The public model id is
# the artifact's identity.model_id ("qwen3.8-27b"), which is what
# ninfer-serve advertises and accepts by default.

{
  config,
  pkgs,
  lib,
  ...
}:

let
  containerName = "ninfer-dockerized-Qwen3.8-27B-NVFP4";
  port = 22545;

  # Model artifact (single file, ~20 GiB) and its host-side checkout.
  modelHostPath = "/models/neroued-Qwen3.8-27B-nvfp4-NInfer";
  modelFile = "qwen3_8_27b_nvfp4.ninfer";
  modelHfRepo = "neroued/Qwen3.8-27B-nvfp4-NInfer";
  # Published SHA-256 (model card / README table / SHA256SUMS).
  modelSha256 = "bb3360522a06e136e0367f5703414d26272b7285c8a6ab6194135c17dbd81b32";

  # Locally built engine image (no registry distribution exists).
  ninferImage = "ninfer:local";
  # Pinned engine source (master @ 2026-08-19); newer than the
  # artifact's minimum_revision (5d2c1f55, see artifact-manifest.json).
  ninferGitRef = "feaf4dd0983fdaeb2ba4c06eec6da350e644fb3a";

  # Public model id: the artifact's identity.model_id.
  servedModelName = "qwen3.8-27b";

  podmanBin = "${pkgs.podman}/bin/podman";

  ninferPkg = pkgs.writeShellApplication {
    name = containerName;
    runtimeInputs = [
      pkgs.coreutils
      pkgs.git
      pkgs.podman
      (pkgs.python3.withPackages (ps: [ ps.huggingface-hub ]))
    ];
    text = ''
      set -euo pipefail

      # First argument is the optional host port; everything else
      # is passed verbatim to the ninfer-serve CLI.
      EXTRA_ARGS=()
      if [ $# -gt 0 ]; then
        HOST_PORT="$1"
        shift
        EXTRA_ARGS=("$@")
      else
        HOST_PORT="''${HOST_PORT:-${toString port}}"
      fi

      # Host-side model checkout (directory containing the artifact).
      MODEL_HOST_PATH="''${MODEL_HOST_PATH:-${modelHostPath}}"
      # Registered artifact inside that directory.
      MODEL_FILE="''${MODEL_FILE:-${modelFile}}"

      # Locally built NInfer image; built from pinned GitHub source on
      # first use (there is no registry distribution).
      NINFER_IMAGE="''${NINFER_IMAGE:-${ninferImage}}"
      NINFER_GIT_REF="''${NINFER_GIT_REF:-${ninferGitRef}}"
      BUILD_IMAGE="''${BUILD_IMAGE:-1}"

      # Server tuning. Defaults mirror the published Qwen3.8-27B NVFP4
      # RTX 5090 profile: the NVFP4 weights need a 252,928-token context
      # to fit the card; the saturated measurements use INT8 group-64 KV
      # with CUDA Graphs and MTP3 (draft tokens) plus the optimized
      # proposal head.
      MAX_CONTEXT="''${MAX_CONTEXT:-252928}"
      KV_CAPACITY="''${KV_CAPACITY:-auto}"
      MAX_CONCURRENCY="''${MAX_CONCURRENCY:-8}"
      KV_DTYPE="''${KV_DTYPE:-int8}"
      # Speculative decoding backend; set SPEC="" to disable.
      SPEC="''${SPEC-mtp}"
      DRAFT_TOKENS="''${DRAFT_TOKENS:-3}"
      LM_HEAD_DRAFT="''${LM_HEAD_DRAFT:-1}"
      # Vision is disabled by default (like the text-only vLLM variants);
      # the artifact supports it, enable with VISION=1.
      VISION="''${VISION:-0}"
      NO_CUDA_GRAPH="''${NO_CUDA_GRAPH:-0}"
      REMOVE_EXISTING_CONTAINER="''${REMOVE_EXISTING_CONTAINER:-1}"

      # Published SHA-256 of the artifact (model card); empty skips.
      MODEL_SHA256="''${MODEL_SHA256:-${modelSha256}}"
      VERIFY_SHA256="''${VERIFY_SHA256:-1}"

      MODEL_HF_REPO="''${MODEL_HF_REPO:-${modelHfRepo}}"

      if [ -n "$MODEL_HF_REPO" ]; then
        echo "Ensuring model is present: $MODEL_HF_REPO" >&2
        MODEL_DOWNLOAD_PATH="''${MODEL_HOST_PATH#/models/}"
        hf download "$MODEL_HF_REPO" --local-dir "/home/mhuber/models/''${MODEL_DOWNLOAD_PATH}"
      fi

      if [ ! -d "$MODEL_HOST_PATH" ]; then
        echo "Model directory does not exist: $MODEL_HOST_PATH" >&2
        exit 1
      fi

      if [ ! -f "$MODEL_HOST_PATH/$MODEL_FILE" ]; then
        echo "Model artifact does not exist: $MODEL_HOST_PATH/$MODEL_FILE" >&2
        exit 1
      fi

      if [ "$VERIFY_SHA256" = "1" ] && [ -n "$MODEL_SHA256" ]; then
        echo "Verifying artifact checksum (sha256)..." >&2
        echo "$MODEL_SHA256  $MODEL_HOST_PATH/$MODEL_FILE" | sha256sum --check -
      fi

      if ! ${podmanBin} image exists "$NINFER_IMAGE"; then
        if [ "$BUILD_IMAGE" != "1" ]; then
          echo "Image $NINFER_IMAGE not found and BUILD_IMAGE != 1; build it with:" >&2
          echo "  ${podmanBin} build --pull -t $NINFER_IMAGE git+https://github.com/Neroued/ninfer.git#''${NINFER_GIT_REF}:" >&2
          exit 1
        fi
        echo "Building image $NINFER_IMAGE from git+https://github.com/Neroued/ninfer.git#''${NINFER_GIT_REF} (one-time; pulls the CUDA base images and compiles the engine)..." >&2
        ${podmanBin} build --pull --tag "$NINFER_IMAGE" "git+https://github.com/Neroued/ninfer.git#''${NINFER_GIT_REF}:"
      fi

      if [ "$REMOVE_EXISTING_CONTAINER" = "1" ]; then
        if ${podmanBin} ps -a --format '{{.Names}}' | grep -Fxq "${containerName}"; then
          ${podmanBin} rm -f "${containerName}" >/dev/null
        fi
      fi

      args=(
        ${podmanBin} run
        --rm
        --device nvidia.com/gpu=all
        --name "${containerName}"
        --ipc=host
        -p "$HOST_PORT:8080"
        -v "$MODEL_HOST_PATH:/models:ro"
        "$NINFER_IMAGE"
        ninfer-serve "/models/$MODEL_FILE"
        --host 0.0.0.0
        --port 8080
        --max-context "$MAX_CONTEXT"
        --kv-capacity "$KV_CAPACITY"
        --max-concurrency "$MAX_CONCURRENCY"
        --kv-dtype "$KV_DTYPE"
      )

      # Speculative decoding: MTP with draft tokens; the optimized
      # proposal head requires a selected spec backend.
      if [ -n "$SPEC" ]; then
        args+=(--spec "$SPEC" --draft-tokens "$DRAFT_TOKENS")
        if [ "$LM_HEAD_DRAFT" = "1" ]; then
          args+=(--lm-head-draft)
        fi
      fi

      if [ "$VISION" = "1" ]; then
        args+=(--vision)
      fi

      if [ "$NO_CUDA_GRAPH" = "1" ]; then
        args+=(--no-cuda-graph)
      fi

      # Append any positional arguments beyond the port.
      if [ ''${#EXTRA_ARGS[@]} -gt 0 ]; then
        args+=("''${EXTRA_ARGS[@]}")
      fi

      echo "Starting NInfer container:"
      echo "  model:              $MODEL_HOST_PATH/$MODEL_FILE"
      echo "  served model id:    ${servedModelName}"
      echo "  endpoint:           http://localhost:$HOST_PORT/v1"
      echo "  image:              $NINFER_IMAGE"
      echo "  max context:        $MAX_CONTEXT (kv: $KV_CAPACITY, $KV_DTYPE)"
      echo "  concurrency:        $MAX_CONCURRENCY (spec: ''${SPEC:-off})"
      echo

      set -x
      exec "''${args[@]}"
    '';
  };
in
{
  imports = [
    {
      # Self-contained defaults (consistent with the vLLM siblings);
      # the llama-swap unit's Podman and NVIDIA CDI start dependencies
      # are declared in myconfig.ai.vllm/default.nix so they are not
      # duplicated.
      virtualisation.podman.enable = lib.mkDefault true;
      # NVIDIA CDI specs come from the same generator that served Docker.
      hardware.nvidia-container-toolkit.enable = lib.mkDefault true;
    }
  ];

  config = {
    environment.systemPackages = [ ninferPkg ];

    services.llama-swap.settings.models = {
      "ninfer:Qwen3.8-27B-NVFP4" = {
        cmd = "${ninferPkg}/bin/${containerName}";
        proxy = "http://127.0.0.1:${toString port}";
        name = "NInfer Qwen3.8-27B NVFP4";
        # The public model id the container accepts (artifact
        # identity.model_id), not the llama-swap key.
        useModelName = servedModelName;
        # Aliases must be unique across all llama-swap models (the
        # config loader hard-fails on duplicates), so none of the
        # vLLM-variant aliases (localhost:22545, vllm:*) are reused.
        aliases = [
          "ninfer"
          "ninfer:qwen3.8-27b"
        ];
        cmdStop = "${podmanBin} stop ${containerName}";
        ttl = 0;
      };
    };

    home-manager.sharedModules = [
      {
        programs.aichat.settings.clients = [
          {
            type = "openai-compatible";
            name = "ninfer";
            # Points straight at the container port (like the vllm
            # client), so the model name is the artifact's public
            # model id, not a llama-swap alias.
            api_base = "http://localhost:${toString port}/v1";
            models = [
              { name = servedModelName; }
            ];
          }
        ];
      }
    ];
  };
}
