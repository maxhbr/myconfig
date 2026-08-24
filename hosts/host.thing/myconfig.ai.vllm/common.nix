# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# Common vLLM container launch-script factory (Docker or Podman).
# Provides mkCudaVllmDockerized and mkRocmVllmDockerized. Both backend
# wrappers default containerRuntime to "podman" (GPU via CDI); pass
# "docker" per variant where a real Docker daemon is the runtime.

{ pkgs }:

let
  # Build the shared shell-script body for a vLLM container (Docker or Podman).
  # $1 is the optional host port; everything else is passed to vLLM CLI.
  #
  # Backend-specific parts are injected via:
  #   backendDockerImage   – default image (overridable via DOCKER_IMAGE env var)
  #   backendDockerRunArgs – shell fragment for GPU device/group/env flags
  #   containerRuntime     – "docker" or "podman"
  #
  # All remaining parameters are vLLM tuning knobs and model metadata.
  mkVllmDockerized =
    {
      containerRuntime ? "docker", # "docker" or "podman"
      # --- Backend-specific ---
      backendDockerImage, # e.g. "docker.io/vllm/vllm-openai:v0.27.1"
      backendDockerRunArgs, # shell fragment (inside the args=(...) block)
      backendModelArg, # shell fragment for passing model path (e.g. "/model" or "--model" "/model")

      # --- Common model parameters ---
      modelHostPath,
      servedModelName,
      containerName,
      port,
      maxModelLen ? 185024,
      extraConfig ? { },

      # Optional vLLM tuning overrides (null = use script default)
      dtype ? null,
      gpuMemoryUtilization ? null,
      maxNumSeqs ? null,
      kvCacheDtype ? null,
      reasoningParser ? null,
      compilationConfig ? null,
      speculativeConfig ? null,
      enableEnforceEager ? null, # null = keep existing default (0)

      # Hugging Face repo for on-demand download (null = skip download)
      modelHfRepo ? null,
      # Docker image override; null = backend default (DOCKER_IMAGE env still
      # wins).
      dockerImage ? null,
      # Container-tools layers to lazily expose onto the launched process's
      # PATH; entries are subdirectory names of the host-persistent layer
      # pool /home/mhuber/models/pkgs/docker-layers/<name>/ (directory
      # names derive from the local Nix package that produces the tools, e.g.
      # "vram"). No pulls happen here; the containerRuntime binary's own
      # runtime is always resolvable via static runtimeInputs, so host-nix
      # store rotation cannot break the launcher itself.
      runtimeLayers ? [ ],
      # Optional vLLM --quantization value (e.g. "modelopt"); null = skip the flag
      quantization ? null,
      # Toggle prefix caching (null = engine default, 0 = --no-enable-prefix-caching)
      enablePrefixCaching ? null,
      # Optional vLLM --generation-config value (e.g. "vllm"); null = skip the flag
      generationConfig ? null,
    }:
    let
      img = if dockerImage != null then dockerImage else backendDockerImage;
      runtime = pkgs.${containerRuntime};
      runtimeBin = "${runtime}/bin/${containerRuntime}";
      # Space-separated pool-layer names for the script's lazy PATH block.
      layerNamesForText = builtins.concatStringsSep " " runtimeLayers;

      vllmPkg = pkgs.writeShellApplication {
        name = containerName;

        runtimeInputs = [
          pkgs.coreutils
          runtime
          (pkgs.python3.withPackages (ps: [ ps.huggingface-hub ]))
        ];

        text = ''
          set -euo pipefail

          # First argument is the optional host port; everything else
          # is passed verbatim to the vLLM CLI.
          EXTRA_ARGS=()
          if [ $# -gt 0 ]; then
            HOST_PORT="$1"
            shift
            EXTRA_ARGS=("$@")
          else
            HOST_PORT="''${HOST_PORT:-${toString port}}"
          fi

          # Host-side model checkout.
          MODEL_HOST_PATH="''${MODEL_HOST_PATH:-${modelHostPath}}"

          # Container/vLLM settings.
          DOCKER_IMAGE="''${DOCKER_IMAGE:-${img}}"

          # vLLM model/server settings.
          DTYPE="''${DTYPE-${if dtype != null then toString dtype else "bfloat16"}}"
          GPU_MEMORY_UTILIZATION="''${GPU_MEMORY_UTILIZATION-${
            if gpuMemoryUtilization != null then toString gpuMemoryUtilization else "0.93"
          }}"
          MAX_NUM_SEQS="''${MAX_NUM_SEQS-${if maxNumSeqs != null then toString maxNumSeqs else "1"}}"
          MAX_NUM_BATCHED_TOKENS="''${MAX_NUM_BATCHED_TOKENS:-1024}"
          KV_CACHE_DTYPE="''${KV_CACHE_DTYPE-${if kvCacheDtype != null then kvCacheDtype else ""}}"
          REASONING_PARSER="''${REASONING_PARSER-${if reasoningParser != null then reasoningParser else ""}}"
          COMPILATION_CONFIG="''${COMPILATION_CONFIG-${
            if compilationConfig != null then compilationConfig else ""
          }}"
          SPECULATIVE_CONFIG="''${SPECULATIVE_CONFIG-${
            if speculativeConfig != null then speculativeConfig else ""
          }}"
          QUANTIZATION="''${QUANTIZATION-${if quantization != null then quantization else ""}}"
          ENABLE_PREFIX_CACHING="''${ENABLE_PREFIX_CACHING-${
            if enablePrefixCaching != null then (if enablePrefixCaching then "1" else "0") else ""
          }}"
          GENERATION_CONFIG="''${GENERATION_CONFIG-${
            if generationConfig != null then generationConfig else ""
          }}"

          # Toggle flags.
          TRUST_REMOTE_CODE="''${TRUST_REMOTE_CODE:-1}"
          LANGUAGE_MODEL_ONLY="''${LANGUAGE_MODEL_ONLY:-1}"
          ENFORCE_EAGER="''${ENFORCE_EAGER-${
            if enableEnforceEager != null then (if enableEnforceEager then "1" else "0") else "0"
          }}"
          REMOVE_EXISTING_CONTAINER="''${REMOVE_EXISTING_CONTAINER:-1}"

          # Tool calling flags (required for "auto" tool_choice in OpenAI API).
          ENABLE_AUTO_TOOL_CHOICE="''${ENABLE_AUTO_TOOL_CHOICE:-1}"
          TOOL_CALL_PARSER="''${TOOL_CALL_PARSER:-qwen3_xml}"

          MODEL_HF_REPO="''${MODEL_HF_REPO:-${if modelHfRepo != null then modelHfRepo else ""}}"

          if [ -n "$MODEL_HF_REPO" ]; then
            echo "Ensuring model is present: $MODEL_HF_REPO" >&2
            MODEL_DOWNLOAD_PATH="''${MODEL_HOST_PATH#/models/}"
            hf download "$MODEL_HF_REPO" --local-dir "/home/mhuber/models/''${MODEL_DOWNLOAD_PATH}"
          fi

          if [ ! -d "$MODEL_HOST_PATH" ]; then
            echo "Model directory does not exist: $MODEL_HOST_PATH" >&2
            exit 1
          fi

          if [ ! -f "$MODEL_HOST_PATH/config.json" ]; then
            echo "Model directory is missing config.json: $MODEL_HOST_PATH" >&2
            exit 1
          fi

          ${
            if runtimeLayers != [ ] then
              ''
                # Container-tools layers: lazily expose bin dirs of these layers
                # under LAYER_POOL (the pool is host-persistent and is curated per
                # package, e.g. docker layers built by a package named `vram`).
                # Missing layers only warn; startup is never blocked by them.
                LAYER_POOL="''${LAYER_POOL:-/home/mhuber/models/pkgs/docker-layers}"
                # shellcheck disable=SC2043
                for _layer_pkg in ${layerNamesForText}; do
                  if [ -d "$LAYER_POOL/$_layer_pkg/bin" ]; then
                    export PATH="$LAYER_POOL/$_layer_pkg/bin:$PATH"
                  else
                    echo "warn: container tools layer '$_layer_pkg' not found under $LAYER_POOL - continuing without it" >&2
                  fi
                done
              ''
            else
              ""
          }

          if [ "$REMOVE_EXISTING_CONTAINER" = "1" ]; then
            if ${runtimeBin} ps -a --format '{{.Names}}' | grep -Fxq "${containerName}"; then
              ${runtimeBin} rm -f "${containerName}" >/dev/null
            fi
          fi

          args=(
            ${runtimeBin} run
            --rm
            ${backendDockerRunArgs}
            --name "${containerName}"
            --ipc=host
            -p "$HOST_PORT:8000"
            -v "$MODEL_HOST_PATH:/model:ro"
            "$DOCKER_IMAGE"
            ${backendModelArg}
            --dtype "$DTYPE"
            --max-model-len ${toString maxModelLen}
            --gpu-memory-utilization "$GPU_MEMORY_UTILIZATION"
            --max-num-seqs "$MAX_NUM_SEQS"
            --max-num-batched-tokens "$MAX_NUM_BATCHED_TOKENS"
          )

          if [ "$TRUST_REMOTE_CODE" = "1" ]; then
            args+=(--trust-remote-code)
          fi

          if [ "$LANGUAGE_MODEL_ONLY" = "1" ]; then
            args+=(--language-model-only)
          fi

          if [ "$ENFORCE_EAGER" = "1" ]; then
            args+=(--enforce-eager)
          fi

          if [ "$ENABLE_AUTO_TOOL_CHOICE" = "1" ]; then
            args+=(--enable-auto-tool-choice)
          fi

          if [ -n "$TOOL_CALL_PARSER" ]; then
            args+=(--tool-call-parser "$TOOL_CALL_PARSER")
          fi

          if [ -n "$KV_CACHE_DTYPE" ]; then
            args+=(--kv-cache-dtype "$KV_CACHE_DTYPE")
          fi

          if [ -n "$REASONING_PARSER" ]; then
            args+=(--reasoning-parser "$REASONING_PARSER")
          fi

          if [ -n "$COMPILATION_CONFIG" ]; then
            args+=(--compilation-config.cudagraph_mode "$COMPILATION_CONFIG")
          fi

          if [ -n "$SPECULATIVE_CONFIG" ]; then
            args+=(--speculative-config "$SPECULATIVE_CONFIG")
          fi

          if [ -n "$QUANTIZATION" ]; then
            args+=(--quantization "$QUANTIZATION")
          fi

          if [ "$ENABLE_PREFIX_CACHING" = "1" ]; then
            args+=(--enable-prefix-caching)
          elif [ "$ENABLE_PREFIX_CACHING" = "0" ]; then
            args+=(--no-enable-prefix-caching)
          fi

          if [ -n "$GENERATION_CONFIG" ]; then
            args+=(--generation-config "$GENERATION_CONFIG")
          fi

          # Append any positional arguments beyond the port.
          if [ ''${#EXTRA_ARGS[@]} -gt 0 ]; then
            args+=("''${EXTRA_ARGS[@]}")
          fi

          if [ -n "''${EXTRA_VLLM_ARGS:-}" ]; then
            # shellcheck disable=SC2206
            extra_args=( $EXTRA_VLLM_ARGS )
            args+=("''${extra_args[@]}")
          fi

          # Always add localhost:$HOST_PORT so callers can address the model
          # generically without knowing the internal model name in advance.
          # Optionally append extra names via EXTRA_SERVED_MODEL_NAMES (space-separated).
          # One --served-model-name flag takes the whole list below (vLLM's
          # flag is multiple-valued); every name addresses the same server.
          all_served_names=("${servedModelName}")
          all_served_names+=("localhost:$HOST_PORT")
          if [ -n "''${EXTRA_SERVED_MODEL_NAMES:-}" ]; then
            # shellcheck disable=SC2206
            all_served_names+=( $EXTRA_SERVED_MODEL_NAMES )
          fi
          args+=(--served-model-name)
          for extra_name in "''${all_served_names[@]}"; do
            args+=("$extra_name")
          done

          echo "Starting vLLM container:"
          echo "  model:              $MODEL_HOST_PATH"
          echo "  served model name:  ${servedModelName}"
          echo "  endpoint:           http://localhost:$HOST_PORT/v1"
          echo "  image:              $DOCKER_IMAGE"
          echo "  gpu utilization:    $GPU_MEMORY_UTILIZATION"
          echo

          set -x
          exec "''${args[@]}"
        '';
      };
    in
    {
      inherit vllmPkg;
      modelConfig = {
        "vllm:${servedModelName}" = {
          cmd = "${vllmPkg}/bin/${containerName}";
          proxy = "http://127.0.0.1:${toString port}";
          name = servedModelName;
          useModelName = servedModelName;
          aliases = [
            servedModelName
            "localhost:${toString port}"
            "vllm:hermes"
            "vllm:opencode"
          ]
          ++ (extraConfig.aliases or [ ]);
          cmdStop = "${runtimeBin} stop ${containerName}";
          ttl = 0;
        };
      };
    };

in
{
  # CUDA convenience wrapper – pins the NVIDIA image and device flags.
  mkCudaVllmDockerized =
    {
      modelHostPath,
      servedModelName,
      containerName,
      port,
      maxModelLen ? 185024,
      extraConfig ? { },
      # This host serves the CUDA variant on Podman (GPU exposure via CDI — the
      # nvidia-container-toolkit cdi-generator provides the specs); a variant
      # that genuinely needs a Docker daemon can pass its own containerRuntime.
      containerRuntime ? "podman",
      # Optional vLLM tuning overrides (null = use script default)
      dtype ? null,
      gpuMemoryUtilization ? null,
      maxNumSeqs ? null,
      kvCacheDtype ? null,
      reasoningParser ? null,
      compilationConfig ? null,
      speculativeConfig ? null,
      enableEnforceEager ? null,
      # Hugging Face repo for on-demand download (null = skip download)
      modelHfRepo ? null,
      dockerImage ? null,
      # Container-tools layers lazily exposed via LAYER_POOL (pool subdir names;
      # see the factory's runtimeLayers doc and the generated script block).
      runtimeLayers ? [ ],
      quantization ? null,
      enablePrefixCaching ? null,
      generationConfig ? null,
    }:
    mkVllmDockerized {
      # Pinned (same version as the ad-hoc server variant 7 was modelled
      # on): the baked flag sets (--speculative-config, --quantization, ...)
      # were tuned against this image; a silent :latest upgrade would pull
      # in a changed flag surface. The DOCKER_IMAGE env override still
      # overrides this per launch.
      backendDockerImage = "docker.io/vllm/vllm-openai:v0.27.1";
      backendDockerRunArgs = "--device nvidia.com/gpu=all";
      backendModelArg = "\"/model\"";
      inherit
        modelHostPath
        servedModelName
        containerName
        port
        maxModelLen
        extraConfig
        containerRuntime
        dtype
        gpuMemoryUtilization
        maxNumSeqs
        kvCacheDtype
        reasoningParser
        compilationConfig
        speculativeConfig
        enableEnforceEager
        modelHfRepo
        dockerImage
        runtimeLayers
        quantization
        enablePrefixCaching
        generationConfig
        ;
    };

  # ROCm convenience wrapper – pins the ROCm image and AMD device flags.
  mkRocmVllmDockerized =
    {
      modelHostPath,
      servedModelName,
      containerName,
      port,
      maxModelLen ? 185024,
      extraConfig ? { },
      containerRuntime ? "podman",
      # Optional vLLM tuning overrides (null = use script default)
      dtype ? null,
      gpuMemoryUtilization ? null,
      maxNumSeqs ? null,
      kvCacheDtype ? null,
      reasoningParser ? null,
      compilationConfig ? null,
      speculativeConfig ? null,
      enableEnforceEager ? null,
      # Hugging Face repo for on-demand download (null = skip download)
      modelHfRepo ? null,
      # ROCm: override GFX version (e.g. "11.5.1")
      rocmOverrideGfxVersion ? "11.5.1",
      dockerImage ? null,
      quantization ? null,
      enablePrefixCaching ? null,
      generationConfig ? null,
    }:
    mkVllmDockerized {
      backendDockerImage = "docker.io/rocm/vllm:rocm7.13.0_gfx1151_ubuntu24.04_py3.13_pytorch_2.10.0_vllm_0.19.1";
      backendDockerRunArgs = ''
        --device /dev/kfd
        --device /dev/dri
        --entrypoint vllm
        -e HSA_OVERRIDE_GFX_VERSION="${rocmOverrideGfxVersion}"
      '';
      backendModelArg = "\"serve\" \"/model\"";
      inherit
        modelHostPath
        servedModelName
        containerName
        port
        maxModelLen
        extraConfig
        containerRuntime
        dtype
        gpuMemoryUtilization
        maxNumSeqs
        kvCacheDtype
        reasoningParser
        compilationConfig
        speculativeConfig
        enableEnforceEager
        modelHfRepo
        dockerImage
        quantization
        enablePrefixCaching
        generationConfig
        ;
    };
}
