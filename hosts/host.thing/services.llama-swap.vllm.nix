{
  config,
  pkgs,
  lib,
  ...
}:

let
  servedModelName = "Qwen3.6-27B-NVFP4";
  containerName = "vllm-dockerized-${servedModelName}";
  port = 22548;
  docker = "${pkgs.docker}/bin/docker";
  vllmDockerizedQwen36_27B_NVFP4 = pkgs.writeShellApplication {
    name = containerName;

    runtimeInputs = [
      pkgs.coreutils
    ];

    text = ''
      set -euo pipefail

      # Host-side model checkout.
      MODEL_HOST_PATH="''${MODEL_HOST_PATH:-/models/${servedModelName}}"

      # Docker/vLLM settings.
      DOCKER_IMAGE="''${DOCKER_IMAGE:-docker.io/vllm/vllm-openai:latest}"
      HOST_PORT="''${HOST_PORT:-22548}"

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
        --gpus all
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
  imports = [
    {
      virtualisation.docker.enable = true;
      systemd.services.llama-swap = {
        wants = [
          "docker.service"
          "docker.socket"
        ];
        after = [
          "docker.service"
          "docker.socket"
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
      vllmDockerizedQwen36_27B_NVFP4
    ];
    services.llama-swap.settings.models = {
      "vllm:${servedModelName}" = {
        cmd = "${vllmDockerizedQwen36_27B_NVFP4}/bin/${containerName}";
        proxy = "http://127.0.0.1:${toString port}";
        name = servedModelName;
        useModelName = servedModelName;
        aliases = [
          servedModelName
          "vllm:hermes"
          "vllm:opencode"
        ];
        cmdStop = "${docker} stop ${containerName}";
        ttl = 0;
      };
    };
  };
}
