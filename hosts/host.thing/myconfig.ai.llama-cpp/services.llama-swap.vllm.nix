{
  config,
  pkgs,
  lib,
  ...
}:

let
  docker = "${pkgs.docker}/bin/docker";

  # Build a vLLM Docker launch script for a given model variant.
  mkVllmDockerized =
    {
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
      enableEnforceEager ? null, # null = keep existing default (1)
      # Hugging Face repo for on-demand download (null = skip download)
      modelHfRepo ? null,
    }:
    let
      vllmPkg = pkgs.writeShellApplication {
        name = containerName;

        runtimeInputs = [
          pkgs.coreutils
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

          # Docker/vLLM settings.
          DOCKER_IMAGE="''${DOCKER_IMAGE:-docker.io/vllm/vllm-openai:latest}"

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

          # Toggle flags.
          TRUST_REMOTE_CODE="''${TRUST_REMOTE_CODE:-1}"
          LANGUAGE_MODEL_ONLY="''${LANGUAGE_MODEL_ONLY:-1}"
          ENFORCE_EAGER="''${ENFORCE_EAGER-${
            if enableEnforceEager != null then (if enableEnforceEager then "1" else "0") else "1"
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

          if [ "$REMOVE_EXISTING_CONTAINER" = "1" ]; then
            if ${docker} ps -a --format '{{.Names}}' | grep -Fxq "${containerName}"; then
              ${docker} rm -f "${containerName}" >/dev/null
            fi
          fi

          args=(
            ${docker} run
            --rm
            --device nvidia.com/gpu=all
            --name "${containerName}"
            --ipc=host
            -p "$HOST_PORT:8000"
            -v "$MODEL_HOST_PATH:/model:ro"
            "$DOCKER_IMAGE"
            "/model"
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
          # Order matters: the LAST --served-model-name becomes the model id.
          all_served_names=("localhost:$HOST_PORT")
          if [ -n "''${EXTRA_SERVED_MODEL_NAMES:-}" ]; then
            # shellcheck disable=SC2206
            all_served_names+=( $EXTRA_SERVED_MODEL_NAMES )
          fi
          all_served_names+=("${servedModelName}")
          for extra_name in "''${all_served_names[@]}"; do
            args+=(--served-model-name "$extra_name")
          done

          echo "Starting vLLM Docker container:"
          echo "  model:              $MODEL_HOST_PATH"
          echo "  served model name:  ${servedModelName}"
          echo "  endpoint:           http://localhost:$HOST_PORT/v1"
          echo "  docker image:       $DOCKER_IMAGE"
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
            "vllm:hermes"
            "vllm:opencode"
          ]
          ++ (extraConfig.aliases or [ ]);
          cmdStop = "${docker} stop ${containerName}";
          ttl = 0;
        };
      };
    };

  # --- Variant 1: Qwen3.6-27B-NVFP4 (base) ---
  vllmQwen36_27B_NVFP4 = mkVllmDockerized {
    modelHostPath = "/models/unsloth-Qwen3.6-27B-NVFP4";
    modelHfRepo = "unsloth/Qwen3.6-27B-NVFP4";
    servedModelName = "Qwen3.6-27B-NVFP4";
    containerName = "vllm-dockerized-Qwen3.6-27B-NVFP4";
    port = 22548;
    maxModelLen = 185024;
    extraConfig = { };
  };

  # --- Variant 2: Qwen3.6-27B-Text-NVFP4-MTP (multi-token prediction) ---
  vllmQwen36_27B_Text_NVFP4_MTP = mkVllmDockerized {
    modelHostPath = "/models/unsloth-Qwen3.6-27B-Text-NVFP4-MTP";
    modelHfRepo = "unsloth/Qwen3.6-27B-Text-NVFP4-MTP";
    servedModelName = "Qwen3.6-27B-Text-NVFP4-MTP";
    containerName = "vllm-dockerized-Qwen3.6-27B-Text-NVFP4-MTP";
    port = 22548;
    maxModelLen = 185024;
    extraConfig = {
      aliases = [
        "vllm:mtp"
      ];
    };
  };

  # --- Variant 3: Qwen3.6-27B-int4-AutoRound (Intel AutoRound int4) ---
  vllmQwen36_27B_int4_AutoRound = mkVllmDockerized {
    modelHostPath = "/models/Intel-Qwen3.6-27B-int4-AutoRound";
    modelHfRepo = "Intel/Qwen3.6-27B-int4-AutoRound";
    servedModelName = "Qwen3.6-27B-int4-AutoRound";
    containerName = "vllm-dockerized-Qwen3.6-27B-int4-AutoRound";
    port = 22548;
    maxModelLen = 185024;
    extraConfig = {
      aliases = [
        "vllm:autoround"
      ];
    };
  };

  # --- Variant 4: Qwen3.6-27B-int4-AutoRound (Lorbus, MTP + reasoning) ---
  vllmQwen36_27B_int4_AutoRound_Lorbus = mkVllmDockerized {
    modelHostPath = "/models/Lorbus-Qwen3.6-27B-int4-AutoRound";
    modelHfRepo = "Lorbus/Qwen3.6-27B-int4-AutoRound";
    servedModelName = "Qwen3.6-27B-int4-AutoRound-Lorbus";
    containerName = "vllm-dockerized-Qwen3.6-27B-int4-AutoRound-Lorbus";
    port = 22548;
    maxModelLen = 262144;
    dtype = "half";
    gpuMemoryUtilization = 0.85;
    maxNumSeqs = 3;
    kvCacheDtype = "tq-t4nc";
    reasoningParser = "qwen3";
    compilationConfig = "none";
    speculativeConfig = "{\"method\": \"mtp\", \"num_speculative_tokens\": 1}";
    enableEnforceEager = false;
    extraConfig = {
      aliases = [
        "vllm:lorbus"
      ];
    };
  };
in
{
  imports = [
    {
      virtualisation.docker.enable = lib.mkDefault true;
      hardware.nvidia-container-toolkit.enable = lib.mkDefault true;
      systemd.services.llama-swap = {
        wants = [
          "docker.service"
          "docker.socket"
          "nvidia-container-toolkit-cdi-generator.service"
        ];
        after = [
          "docker.service"
          "docker.socket"
          "nvidia-container-toolkit-cdi-generator.service"
        ];

        serviceConfig = {
          SupplementaryGroups = [ "docker" ];
          # Only needed if the service uses a dedicated non-root user:
          # User = "llama-swap";
        };

        environment = {
          DOCKER_HOST = "unix:///var/run/docker.sock";
        };
      };
    }
  ];
  config = {
    environment.systemPackages = [
      vllmQwen36_27B_NVFP4.vllmPkg
      vllmQwen36_27B_Text_NVFP4_MTP.vllmPkg
      vllmQwen36_27B_int4_AutoRound.vllmPkg
      vllmQwen36_27B_int4_AutoRound_Lorbus.vllmPkg
    ];
    services.llama-swap.settings.models =
      vllmQwen36_27B_NVFP4.modelConfig
      // vllmQwen36_27B_Text_NVFP4_MTP.modelConfig
      // vllmQwen36_27B_int4_AutoRound.modelConfig
      // vllmQwen36_27B_int4_AutoRound_Lorbus.modelConfig;
    home-manager.sharedModules = [
      {
        programs.aichat.settings.clients = [
          {
            type = "openai-compatible";
            name = "vllm";
            api_base = "http://localhost:22548/v1";
            models = [
              { name = "Qwen3.6-27B-NVFP4"; }
              { name = "Qwen3.6-27B-Text-NVFP4-MTP"; }
              { name = "Qwen3.6-27B-int4-AutoRound"; }
              { name = "Qwen3.6-27B-int4-AutoRound-Lorbus"; }
            ];
          }
        ];
      }
    ];
  };
}
