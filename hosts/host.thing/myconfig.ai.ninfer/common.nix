# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# Common NInfer Docker/Podman launch-script factory.
#
# NInfer (https://github.com/Neroued/ninfer) is a from-scratch C++/CUDA
# inference engine for a closed set of registered Qwen checkpoints on a
# single NVIDIA GeForce RTX 5090. It ships a Dockerfile but, as of this
# writing, does not publish a prebuilt image to any registry — see
# https://github.com/Neroued/ninfer/blob/master/Dockerfile and the "Docker"
# section of the README. The default image reference below therefore
# assumes a locally built image tagged `ninfer:local` (as documented
# upstream: `docker build --tag ninfer:local .`); override via the
# `DOCKER_IMAGE` env var if a different tag/registry is used.
#
# Each `.ninfer` artifact is a single file (not a directory checkout like
# vLLM's Hugging Face repos), so the model is addressed as a host directory
# (`modelHostDir`, bind-mounted read-only to `/models` in the container)
# plus a `modelFilename` inside it.

{ pkgs }:

let
  mkNinferDockerized =
    {
      containerRuntime ? "podman", # "docker" or "podman"

      # --- Model ---
      modelHostDir, # host directory containing the `.ninfer` artifact
      modelFilename, # e.g. "qwen3_8_27b_nvfp4.ninfer"
      servedModelName,
      containerName,
      port,

      # --- NInfer server tuning knobs (null = use script default) ---
      maxContext ? 252928,
      kvCapacity ? "auto",
      maxConcurrency ? 1,
      spec ? "mtp",
      draftTokens ? 3,
      lmHeadDraft ? true,
      vision ? false,

      # Hugging Face repo + filename for on-demand download (null = skip)
      modelHfRepo ? null,

      extraConfig ? { },
    }:
    let
      runtime = pkgs.${containerRuntime};
      runtimeBin = "${runtime}/bin/${containerRuntime}";

      ninferPkg = pkgs.writeShellApplication {
        name = containerName;

        runtimeInputs = [
          pkgs.coreutils
          runtime
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

          # Host-side model checkout (directory holding the .ninfer file).
          MODEL_HOST_DIR="''${MODEL_HOST_DIR:-${modelHostDir}}"
          MODEL_FILENAME="''${MODEL_FILENAME:-${modelFilename}}"

          # Container settings.
          DOCKER_IMAGE="''${DOCKER_IMAGE:-ninfer:local}"

          # NInfer server settings.
          MAX_CONTEXT="''${MAX_CONTEXT:-${toString maxContext}}"
          KV_CAPACITY="''${KV_CAPACITY:-${kvCapacity}}"
          MAX_CONCURRENCY="''${MAX_CONCURRENCY:-${toString maxConcurrency}}"
          SPEC="''${SPEC:-${spec}}"
          DRAFT_TOKENS="''${DRAFT_TOKENS:-${toString draftTokens}}"
          LM_HEAD_DRAFT="''${LM_HEAD_DRAFT:-${if lmHeadDraft then "1" else "0"}}"
          VISION="''${VISION:-${if vision then "1" else "0"}}"

          REMOVE_EXISTING_CONTAINER="''${REMOVE_EXISTING_CONTAINER:-1}"

          MODEL_HF_REPO="''${MODEL_HF_REPO:-${if modelHfRepo != null then modelHfRepo else ""}}"

          if [ -n "$MODEL_HF_REPO" ]; then
            echo "Ensuring model is present: $MODEL_HF_REPO/$MODEL_FILENAME" >&2
            hf download "$MODEL_HF_REPO" "$MODEL_FILENAME" --local-dir "$MODEL_HOST_DIR"
          fi

          if [ ! -d "$MODEL_HOST_DIR" ]; then
            echo "Model directory does not exist: $MODEL_HOST_DIR" >&2
            exit 1
          fi

          if [ ! -f "$MODEL_HOST_DIR/$MODEL_FILENAME" ]; then
            echo "Model artifact is missing: $MODEL_HOST_DIR/$MODEL_FILENAME" >&2
            exit 1
          fi

          if [ "$REMOVE_EXISTING_CONTAINER" = "1" ]; then
            if ${runtimeBin} ps -a --format '{{.Names}}' | grep -Fxq "${containerName}"; then
              ${runtimeBin} rm -f "${containerName}" >/dev/null
            fi
          fi

          args=(
            ${runtimeBin} run
            --rm
            --device nvidia.com/gpu=all
            --name "${containerName}"
            --ipc=host
            -p "$HOST_PORT:8080"
            -v "$MODEL_HOST_DIR:/models:ro"
            "$DOCKER_IMAGE"
            ninfer-serve "/models/$MODEL_FILENAME"
            --host 0.0.0.0
            --port 8080
            --max-context "$MAX_CONTEXT"
            --kv-capacity "$KV_CAPACITY"
            --max-concurrency "$MAX_CONCURRENCY"
          )

          if [ -n "$SPEC" ]; then
            args+=(--spec "$SPEC" --draft-tokens "$DRAFT_TOKENS")
          fi

          if [ "$LM_HEAD_DRAFT" = "1" ]; then
            args+=(--lm-head-draft)
          fi

          if [ "$VISION" = "1" ]; then
            args+=(--vision)
          fi

          args+=(--model-id "${servedModelName}")

          # Append any positional arguments beyond the port.
          if [ ''${#EXTRA_ARGS[@]} -gt 0 ]; then
            args+=("''${EXTRA_ARGS[@]}")
          fi

          if [ -n "''${EXTRA_NINFER_ARGS:-}" ]; then
            # shellcheck disable=SC2206
            extra_args=( $EXTRA_NINFER_ARGS )
            args+=("''${extra_args[@]}")
          fi

          echo "Starting NInfer container:"
          echo "  model:              $MODEL_HOST_DIR/$MODEL_FILENAME"
          echo "  served model name:  ${servedModelName}"
          echo "  endpoint:           http://localhost:$HOST_PORT/v1"
          echo "  container image:    $DOCKER_IMAGE"
          echo

          set -x
          exec "''${args[@]}"
        '';
      };
    in
    {
      inherit ninferPkg;
      modelConfig = {
        "ninfer:${servedModelName}" = {
          cmd = "${ninferPkg}/bin/${containerName}";
          proxy = "http://127.0.0.1:${toString port}";
          name = servedModelName;
          useModelName = servedModelName;
          aliases = [
            servedModelName
          ]
          ++ (extraConfig.aliases or [ ]);
          cmdStop = "${runtimeBin} stop ${containerName}";
          ttl = 0;
        };
      };
    };
in
{
  inherit mkNinferDockerized;
}
