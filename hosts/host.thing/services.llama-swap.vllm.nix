{
  config,
  pkgs,
  lib,
  ...
}:

let
  # Shared Docker/vLLM helpers
  docker = "${pkgs.docker}/bin/docker";

  # ---- Model 1: Qwen3.6-27B-NVFP4 (original) ----
  servedModelName1 = "Qwen3.6-27B-NVFP4";
  containerName1 = "vllm-dockerized-${servedModelName1}";
  port1 = 22548;
  vllmDockerizedQwen36_27B_NVFP4 = pkgs.writeShellApplication {
    name = containerName1;

    runtimeInputs = [
      pkgs.coreutils
    ];

    text = ''
      set -euo pipefail

      # Host-side model checkout.
      MODEL_HOST_PATH="''${MODEL_HOST_PATH:-/models/${servedModelName1}}"

      # Docker/vLLM settings.
      DOCKER_IMAGE="''${DOCKER_IMAGE:-docker.io/vllm/vllm-openai:latest}"
      HOST_PORT="''${HOST_PORT:-${toString port1}}"

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
        if ${docker} ps -a --format '{{.Names}}' | grep -Fxq "${containerName1}"; then
          ${docker} rm -f "${containerName1}" >/dev/null
        fi
      fi

      args=(
        ${docker} run
        --rm
        --device nvidia.com/gpu=all
        --name "${containerName1}"
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
        --served-model-name "${servedModelName1}"
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

      echo "Starting vLLM Docker container:"
      echo "  model:              $MODEL_HOST_PATH"
      echo "  served model name:  ${servedModelName1}"
      echo "  endpoint:           http://localhost:$HOST_PORT/v1"
      echo "  docker image:       $DOCKER_IMAGE"
      echo "  gpu utilization:    $GPU_MEMORY_UTILIZATION"
      echo

      set -x
      exec "''${args[@]}"
    '';
  };

  # ---- Model 2: Qwen3.6-27B-Text-NVFP4-MTP (full context, 262k) ----
  servedModelName2 = "Qwen3.6-27B-Text-NVFP4-MTP";
  containerName2 = "vllm-dockerized-${servedModelName2}";
  port2 = 22549;
  vllmDockerizedQwen36_27B_Text_NVFP4_MTP = pkgs.writeShellApplication {
    name = containerName2;

    runtimeInputs = [
      pkgs.coreutils
    ];

    text = ''
      set -euo pipefail

      MODEL_HOST_PATH="''${MODEL_HOST_PATH:-/models/${servedModelName2}}"

      DOCKER_IMAGE="''${DOCKER_IMAGE:-docker.io/vllm/vllm-openai:latest}"
      HOST_PORT="''${HOST_PORT:-${toString port2}}"

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
        if ${docker} ps -a --format '{{.Names}}' | grep -Fxq "${containerName2}"; then
          ${docker} rm -f "${containerName2}" >/dev/null
        fi
      fi

      args=(
        ${docker} run
        --rm
        --device nvidia.com/gpu=all
        --name "${containerName2}"
        --ipc=host
        -p "$HOST_PORT:8000"
        -v "$MODEL_HOST_PATH:/model:ro"
        "$DOCKER_IMAGE"
        "/model"
        --trust-remote-code
        --quantization modelopt
        --language-model-only
        --max-model-len 262144
        --max-num-seqs 2
        --kv-cache-dtype fp8
        --gpu-memory-utilization 0.9
        --reasoning-parser qwen3
        --speculative-config '{"method":"qwen3_5_mtp","num_speculative_tokens":3}'
        --served-model-name "${servedModelName2}"
      )

      if [ -n "''${EXTRA_VLLM_ARGS:-}" ]; then
        # shellcheck disable=SC2206
        extra_args=( $EXTRA_VLLM_ARGS )
        args+=("''${extra_args[@]}")
      fi

      echo "Starting vLLM Docker container:"
      echo "  model:              $MODEL_HOST_PATH"
      echo "  served model name:  ${servedModelName2}"
      echo "  endpoint:           http://localhost:$HOST_PORT/v1"
      echo "  docker image:       $DOCKER_IMAGE"
      echo

      set -x
      exec "''${args[@]}"
    '';
  };

  # ---- Model 3: Qwen3.6-27B-Text-NVFP4-MTP-smaller-context (8k context) ----
  servedModelName3 = "Qwen3.6-27B-Text-NVFP4-MTP-smaller-context";
  containerName3 = "vllm-dockerized-${servedModelName3}";
  port3 = 22550;
  vllmDockerizedQwen36_27B_Text_NVFP4_MTP_smaller = pkgs.writeShellApplication {
    name = containerName3;

    runtimeInputs = [
      pkgs.coreutils
    ];

    text = ''
      set -euo pipefail

      MODEL_HOST_PATH="''${MODEL_HOST_PATH:-/models/Qwen3.6-27B-Text-NVFP4-MTP}"

      DOCKER_IMAGE="''${DOCKER_IMAGE:-docker.io/vllm/vllm-openai:latest}"
      HOST_PORT="''${HOST_PORT:-${toString port3}}"

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
        if ${docker} ps -a --format '{{.Names}}' | grep -Fxq "${containerName3}"; then
          ${docker} rm -f "${containerName3}" >/dev/null
        fi
      fi

      args=(
        ${docker} run
        --rm
        --device nvidia.com/gpu=all
        --name "${containerName3}"
        --ipc=host
        -p "$HOST_PORT:8000"
        -v "$MODEL_HOST_PATH:/model:ro"
        "$DOCKER_IMAGE"
        "/model"
        --trust-remote-code
        --gpu-memory-utilization 0.85
        --max-model-len 8192
        --language-model-only
        --quantization modelopt
        --reasoning-parser qwen3
        --speculative-config '{"method":"qwen3_5_mtp","num_speculative_tokens":3}'
        --served-model-name "${servedModelName3}"
      )

      if [ -n "''${EXTRA_VLLM_ARGS:-}" ]; then
        # shellcheck disable=SC2206
        extra_args=( $EXTRA_VLLM_ARGS )
        args+=("''${extra_args[@]}")
      fi

      echo "Starting vLLM Docker container:"
      echo "  model:              $MODEL_HOST_PATH"
      echo "  served model name:  ${servedModelName3}"
      echo "  endpoint:           http://localhost:$HOST_PORT/v1"
      echo "  docker image:       $DOCKER_IMAGE"
      echo

      set -x
      exec "''${args[@]}"
    '';
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
        };

        environment = {
          DOCKER_HOST = "unix:///var/run/docker.sock";
        };
      };
    }
  ];
  config = {
    environment.systemPackages = [
      vllmDockerizedQwen36_27B_NVFP4
      vllmDockerizedQwen36_27B_Text_NVFP4_MTP
      vllmDockerizedQwen36_27B_Text_NVFP4_MTP_smaller
    ];
    services.llama-swap.settings.models = {
      "vllm:${servedModelName1}" = {
        cmd = "${vllmDockerizedQwen36_27B_NVFP4}/bin/${containerName1}";
        proxy = "http://127.0.0.1:${toString port1}";
        name = servedModelName1;
        useModelName = servedModelName1;
        aliases = [
          servedModelName1
          "vllm:hermes"
          "vllm:opencode"
        ];
        cmdStop = "${docker} stop ${containerName1}";
        ttl = 0;
      };

      "vllm:${servedModelName2}" = {
        cmd = "${vllmDockerizedQwen36_27B_Text_NVFP4_MTP}/bin/${containerName2}";
        proxy = "http://127.0.0.1:${toString port2}";
        name = servedModelName2;
        useModelName = servedModelName2;
        aliases = [
          servedModelName2
          "Qwen3.6-27B-Text-MTP"
        ];
        cmdStop = "${docker} stop ${containerName2}";
        ttl = 0;
      };

      "vllm:${servedModelName3}" = {
        cmd = "${vllmDockerizedQwen36_27B_Text_NVFP4_MTP_smaller}/bin/${containerName3}";
        proxy = "http://127.0.0.1:${toString port3}";
        name = servedModelName3;
        useModelName = servedModelName3;
        aliases = [
          servedModelName3
          "Qwen3.6-27B-Text-MTP-smaller-context"
        ];
        cmdStop = "${docker} stop ${containerName3}";
        ttl = 0;
      };
    };
    home-manager.sharedModules = [
      {
        programs.aichat.settings.clients = [{
          type = "openai-compatible";
          name = "vllm";
          api_base = "http://localhost:22548/v1";
          models = [
            servedModelName1
            servedModelName2
            servedModelName3
          ];
        }];
      }
    ];
  };
}
