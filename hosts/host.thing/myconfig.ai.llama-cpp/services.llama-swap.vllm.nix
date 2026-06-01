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
      extraConfig ? { },
    }:
    let
      vllmPkg = pkgs.writeShellApplication {
        name = containerName;

        runtimeInputs = [
          pkgs.coreutils
        ];

        text = ''
          set -euo pipefail

          # Optional first argument: host port (overrides env / default).
          if [ $# -gt 0 ]; then
            HOST_PORT="$1"
            shift
          else
            HOST_PORT="''${HOST_PORT:-${toString port}}"
          fi

          # Host-side model checkout.
          MODEL_HOST_PATH="''${MODEL_HOST_PATH:-${modelHostPath}}"

          # Docker/vLLM settings.
          DOCKER_IMAGE="''${DOCKER_IMAGE:-docker.io/vllm/vllm-openai:latest}"

          # vLLM model/server settings.
          DTYPE="''${DTYPE:-bfloat16}"
          MAX_MODEL_LEN="''${MAX_MODEL_LEN:-4096}"
          GPU_MEMORY_UTILIZATION="''${GPU_MEMORY_UTILIZATION:-0.93}"
          MAX_NUM_SEQS="''${MAX_NUM_SEQS:-1}"
          MAX_NUM_BATCHED_TOKENS="''${MAX_NUM_BATCHED_TOKENS:-1024}"

          # Toggle flags.
          TRUST_REMOTE_CODE="''${TRUST_REMOTE_CODE:-1}"
          LANGUAGE_MODEL_ONLY="''${LANGUAGE_MODEL_ONLY:-1}"
          ENFORCE_EAGER="''${ENFORCE_EAGER:-1}"
          REMOVE_EXISTING_CONTAINER="''${REMOVE_EXISTING_CONTAINER:-1}"

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
            --max-model-len "$MAX_MODEL_LEN"
            --gpu-memory-utilization "$GPU_MEMORY_UTILIZATION"
            --max-num-seqs "$MAX_NUM_SEQS"
            --max-num-batched-tokens "$MAX_NUM_BATCHED_TOKENS"
            --served-model-name "${servedModelName}"
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

          if [ -n "''${EXTRA_VLLM_ARGS:-}" ]; then
            # shellcheck disable=SC2206
            extra_args=( $EXTRA_VLLM_ARGS )
            args+=("''${extra_args[@]}")
          fi

          # Always add localhost:$HOST_PORT so callers can address the model
          # generically without knowing the internal model name in advance.
          # Optionally append extra names via EXTRA_SERVED_MODEL_NAMES (space-separated).
          extra_names=("localhost:$HOST_PORT")
          if [ -n "''${EXTRA_SERVED_MODEL_NAMES:-}" ]; then
            # shellcheck disable=SC2206
            extra_names+=( $EXTRA_SERVED_MODEL_NAMES )
          fi
          for extra_name in "''${extra_names[@]}"; do
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
      aichatClient = {
        type = "openai-compatible";
        name = "vllm";
        api_base = "http://localhost:${toString port}/v1";
        models = [
          servedModelName
        ];
      };
    };

  # --- Variant 1: Qwen3.6-27B-NVFP4 (base) ---
  vllmQwen36_27B_NVFP4 = mkVllmDockerized {
    modelHostPath = "/models/unsloth-Qwen3.6-27B-NVFP4";
    servedModelName = "Qwen3.6-27B-NVFP4";
    containerName = "vllm-dockerized-Qwen3.6-27B-NVFP4";
    port = 22548;
    extraConfig = { };
  };

  # --- Variant 2: Qwen3.6-27B-Text-NVFP4-MTP (multi-token prediction) ---
  vllmQwen36_27B_Text_NVFP4_MTP = mkVllmDockerized {
    modelHostPath = "/models/unsloth-Qwen3.6-27B-Text-NVFP4-MTP";
    servedModelName = "Qwen3.6-27B-Text-NVFP4-MTP";
    containerName = "vllm-dockerized-Qwen3.6-27B-Text-NVFP4-MTP";
    port = 22549;
    extraConfig = {
      aliases = [
        "vllm:mtp"
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
    ];
    services.llama-swap.settings.models =
      vllmQwen36_27B_NVFP4.modelConfig // vllmQwen36_27B_Text_NVFP4_MTP.modelConfig;
    home-manager.sharedModules = [
      {
        programs.aichat.settings.clients = [
          vllmQwen36_27B_NVFP4.aichatClient
          vllmQwen36_27B_Text_NVFP4_MTP.aichatClient
        ];
      }
    ];
  };
}
