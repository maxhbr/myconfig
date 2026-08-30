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
      pkgs.gnugrep
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
      # Lowered from the cookbook 0.9 to 0.88: the hybrid model's
      # mamba state cache + FP8 KV cache fit at 0.88 (max_mamba_cache_size=8
      # with --mamba-ssm-dtype bfloat16, 1 request), and the extra ~0.6 GB
      # of runtime slack prevents CUDA OOM during inference. At 0.9 the
      # prefill CUDA graph capture (1.38 GB) + Triton kernel JIT (~0.87 GB)
      # consume almost all of the 3.05 GB slack, leaving <0.1 GB free; a
      # 48 MiB activation allocation then OOMs (with ~487 MiB reserved but
      # fragmented). 0.88 raises slack to 3.66 GB while keeping K=8 mamba
      # slots and ~1.6 GB of KV cache.
      MEM_FRACTION_STATIC="''${MEM_FRACTION_STATIC:-0.88}"
      ATTENTION_BACKEND="''${ATTENTION_BACKEND:-flashinfer}"
      MAX_RUNNING_REQUESTS="''${MAX_RUNNING_REQUESTS:-1}"
      CUDA_GRAPH_MAX_BS="''${CUDA_GRAPH_MAX_BS:-1}" # used for --cuda-graph-max-bs-decode
      REASONING_PARSER="''${REASONING_PARSER:-qwen3}"
      TOOL_CALL_PARSER="''${TOOL_CALL_PARSER:-qwen3_coder}"
      # The Qwen3.8-27B-NVFP4 target is a hybrid model with
      # mamba/linear-attention layers that need a per-request state
      # cache (~147 MB/req in the default float32). SGLang's auto-fit
      # resolves the mamba radix cache strategy to "extra_buffer"
      # (overlap schedule is on by default), which raises the per-request
      # slot ratio to 5 (base 3 + 2 ping-pong). With float32 states the
      # mamba budget only fits 3-4 slots, so max_num_reqs = 3//5 = 0 and
      # SGLang aborts with "Hybrid state cache is too small to serve any
      # requests". Halving the state size to bfloat16 (~73 MB/req) lets
      # the auto-fit allocate 8-10 slots (1-2 requests) and leaves
      # ~1.8-2.2 GB for the FP8 KV cache. SGLang recommends this option
      # directly in the error message.
      MAMBA_SSM_DTYPE="''${MAMBA_SSM_DTYPE:-bfloat16}"

      # DFlash2 speculative decoding (the point of the dev-*-dflash2
      # image); set SPECULATIVE=0 to fall back to greedy decoding.
      SPECULATIVE="''${SPECULATIVE:-1}"
      # Number of draft tokens per speculative step. The cookbook default
      # is 8, but the Qwen3.8-27B-NVFP4 target is a hybrid model with
      # mamba/linear-attention layers that need a per-request state cache
      # (~147 MB/req). With D=8 the intermediate mamba state memory
      # (per_req * (1 + D) = ~1.32 GB) exceeds the mamba budget derived
      # from the ~2.4 GB of rest memory after loading both models on the
      # RTX 5090, so SGLang aborts with
      # "Not enough GPU memory for hybrid (mamba/linear-attention) state
      # cache" (max_mamba_cache_size <= 0). D=4 is the highest value
      # that fits at --mem-fraction-static 0.9 (max_mamba_cache_size=1,
      # matching --max-running-requests 1). Raise to 8 only with
      # --mem-fraction-static >= 0.95 (risky: only 5% runtime slack).
      SPECULATIVE_NUM_DRAFT_TOKENS="''${SPECULATIVE_NUM_DRAFT_TOKENS:-4}"

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

      # Reduce PyTorch CUDA allocator fragmentation so the ~487 MiB of
      # reserved-but-unallocated memory becomes usable for inference
      # activations. The prefill CUDA graph capture and Triton kernel JIT
      # leave the cache highly fragmented; without this the allocator
      # cannot satisfy a 48 MiB request even when enough total memory is
      # free.
      args+=(-e PYTORCH_CUDA_ALLOC_CONF=expandable_segments:True)

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
        --cuda-graph-max-bs-decode "$CUDA_GRAPH_MAX_BS"
        --reasoning-parser "$REASONING_PARSER"
        --tool-call-parser "$TOOL_CALL_PARSER"
        --mamba-ssm-dtype "$MAMBA_SSM_DTYPE"
      )

      if [ "$SPECULATIVE" = "1" ]; then
        args+=(
          --speculative-algorithm DFLASH
          --speculative-draft-model-path "$DRAFT_MODEL_PATH"
          --speculative-num-draft-tokens "$SPECULATIVE_NUM_DRAFT_TOKENS"
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
