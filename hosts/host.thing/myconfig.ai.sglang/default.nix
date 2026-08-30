# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# SGLang container launcher for Qwen3.8-27B with DFlash2 speculative
# decoding on the NVIDIA RTX 5090.
#
# Recipe from the SGLang cookbook:
# https://docs.sglang.io/cookbook/autoregressive/Qwen/Qwen3.8-27B
# (RTX 5090 cell -> image lmsysorg/sglang:dev-qwen38-27b-dflash2,
#  DFlash2 speculative decoding: --speculative-algorithm DFLASH with
#  the incoai/Qwen3.8-27B-DFlash2 draft model).
#
# Container runtime: rootless Podman. The NVIDIA GPU is exposed via the
# `--gpus` flag (backed by nvidia-container-toolkit; the CDI equivalent
# would be `--device nvidia.com/gpu=all` as used by the vLLM/NInfer
# siblings). The user's Hugging Face cache is bind-mounted into the
# container so weights are downloaded once and shared with other
# launchers; rootless Podman maps container-root to the invoking user,
# so downloads land in the user's cache with correct ownership.
#
# Deviations from the cookbook command generator:
# - default host port is 22545 (the llama-swap port on this host)
#   instead of the cookbook's 30000,
# - HF_TOKEN is passed through from the environment when set, instead
#   of a placeholder value.

{
  config,
  pkgs,
  lib,
  ...
}:

let
  port = 22545;
  containerName = "sglang-dockerized-Qwen3.8-27B-DFlash2";
  # Pinned like the vLLM image: the baked flag set below was tuned
  # against this image; a silent tag change would pull in a changed
  # flag surface. The DOCKER_IMAGE env var still overrides per launch.
  dockerImage = "docker.io/lmsysorg/sglang:dev-qwen38-27b-dflash2";
  # RTX 5090 NVFP4 checkpoint (cookbook modelNames table);
  # MODEL_PATH overrides per launch.
  modelPath = "RadixArk/Qwen3.8-27B-NVFP4";
  # DFlash2 draft checkpoint (cookbook speculative options).
  draftModelPath = "incoai/Qwen3.8-27B-DFlash2";

  sglangPkg = pkgs.writeShellApplication {
    name = "sglang-qwen38-27b-dflash2";

    runtimeInputs = [
      pkgs.coreutils
      pkgs.grep
      pkgs.podman
    ];

    text = ''
      set -euo pipefail

      # First argument is the optional host port; everything else is
      # passed verbatim to `sglang serve`.
      EXTRA_ARGS=()
      if [ $# -gt 0 ]; then
        HOST_PORT="$1"
        shift
        EXTRA_ARGS=("$@")
      else
        HOST_PORT="''${HOST_PORT:-${toString port}}"
      fi

      # Container settings.
      DOCKER_IMAGE="''${DOCKER_IMAGE:-${dockerImage}}"
      CONTAINER_NAME="''${CONTAINER_NAME:-${containerName}}"
      REMOVE_EXISTING_CONTAINER="''${REMOVE_EXISTING_CONTAINER:-1}"

      # GPU exposure (word-split; the CDI equivalent is
      # `--device nvidia.com/gpu=all`).
      GPU_ARGS="''${GPU_ARGS:---gpus all}"

      # Model settings (HF repo ids resolved against the shared HF
      # cache; the container downloads missing weights on first run).
      MODEL_PATH="''${MODEL_PATH:-${modelPath}}"
      DRAFT_MODEL_PATH="''${DRAFT_MODEL_PATH:-${draftModelPath}}"

      # sglang server settings (recipe defaults for the RTX 5090 cell).
      KV_CACHE_DTYPE="''${KV_CACHE_DTYPE:-fp8_e4m3}"
      MEM_FRACTION_STATIC="''${MEM_FRACTION_STATIC:-0.9}"
      ATTENTION_BACKEND="''${ATTENTION_BACKEND:-flashinfer}"
      MAX_RUNNING_REQUESTS="''${MAX_RUNNING_REQUESTS:-1}"
      CUDA_GRAPH_MAX_BS="''${CUDA_GRAPH_MAX_BS:-1}"
      REASONING_PARSER="''${REASONING_PARSER:-qwen3}"
      TOOL_CALL_PARSER="''${TOOL_CALL_PARSER:-qwen3_coder}"

      # DFlash2 speculative decoding (the point of the dev-*-dflash2
      # image); set SPECULATIVE=0 to fall back to greedy decoding.
      SPECULATIVE="''${SPECULATIVE:-1}"

      # Shared Hugging Face cache (the cookbook mount target is
      # /root/.cache/huggingface inside the container).
      HF_CACHE="''${HF_CACHE:-$HOME/.cache/huggingface}"

      if [ "$REMOVE_EXISTING_CONTAINER" = "1" ]; then
        if podman ps -a --format '{{.Names}}' | grep -Fxq "$CONTAINER_NAME"; then
          podman rm -f "$CONTAINER_NAME" >/dev/null
        fi
      fi

      args=(
        podman run
        --rm
        --shm-size 32g
        --ipc=host
        --name "$CONTAINER_NAME"
        -p "$HOST_PORT:$HOST_PORT"
        -v "$HF_CACHE:/root/.cache/huggingface"
      )

      # shellcheck disable=SC2206
      gpu_args=( $GPU_ARGS )
      args+=("''${gpu_args[@]}")

      if [ -n "''${HF_TOKEN:-}" ]; then
        args+=(-e HF_TOKEN)
      fi

      args+=(
        "$DOCKER_IMAGE"
        sglang serve
        --trust-remote-code
        --model-path "$MODEL_PATH"
        --host 0.0.0.0
        --port "$HOST_PORT"
        --kv-cache-dtype "$KV_CACHE_DTYPE"
        --mem-fraction-static "$MEM_FRACTION_STATIC"
        --attention-backend "$ATTENTION_BACKEND"
        --max-running-requests "$MAX_RUNNING_REQUESTS"
        --cuda-graph-max-bs "$CUDA_GRAPH_MAX_BS"
        --reasoning-parser "$REASONING_PARSER"
        --tool-call-parser "$TOOL_CALL_PARSER"
      )

      if [ "$SPECULATIVE" = "1" ]; then
        args+=(
          --speculative-algorithm DFLASH
          --speculative-draft-model-path "$DRAFT_MODEL_PATH"
          --speculative-num-draft-tokens 8
        )
      fi

      # Append any positional arguments beyond the port.
      if [ ''${#EXTRA_ARGS[@]} -gt 0 ]; then
        args+=("''${EXTRA_ARGS[@]}")
      fi

      echo "Starting SGLang container:"
      echo "  model:              $MODEL_PATH"
      echo "  draft model:        $DRAFT_MODEL_PATH (DFLASH)"
      echo "  endpoint:           http://localhost:$HOST_PORT/v1"
      echo "  image:              $DOCKER_IMAGE"
      echo "  hf cache:           $HF_CACHE"
      echo

      set -x
      exec "''${args[@]}"
    '';
  };
in
{
  imports = [
    {
      # Self-contained defaults (consistent with the vLLM/NInfer
      # siblings); the llama-swap unit's Podman and NVIDIA CDI start
      # dependencies are declared in ../myconfig.ai.vllm/default.nix
      # so they are not duplicated.
      virtualisation.podman.enable = lib.mkDefault true;
      hardware.nvidia-container-toolkit.enable = lib.mkDefault true;
    }
  ];

  config = {
    environment.systemPackages = [ sglangPkg ];
  };
}
