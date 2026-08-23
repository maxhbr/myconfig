# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# Common NInfer Podman launch-script factory.
#
# NInfer (https://github.com/Neroued/ninfer) is a from-scratch C++/CUDA
# inference engine for a closed set of registered Qwen checkpoints on a
# single NVIDIA GeForce RTX 5090. It ships a Dockerfile but, as of this
# writing, does not publish a prebuilt image to any registry — see
# https://github.com/Neroued/ninfer/blob/master/Dockerfile and the "Docker"
# section of the README. The default image reference below therefore
# assumes a locally built image tagged `ninfer:local`; when that image is
# missing, the launch script itself builds it from a pinned upstream git
# revision (`podman build git+https://github.com/Neroued/ninfer.git#<ref>:`)
# so the variant is turnkey rather than requiring an out-of-band build step.
#
# Each `.ninfer` artifact is a single file (not a directory checkout like
# vLLM's Hugging Face repos), so the model is addressed as a host directory
# (`modelHostDir`, bind-mounted read-only to `/models` in the container)
# plus a `modelFilename` inside it. When the model card publishes a SHA-256
# for the artifact, pass it as `modelSha256` so the script verifies the
# download before every launch.

{ pkgs }:

let
  mkNinferDockerized =
    {
      containerRuntime ? "podman", # "docker" or "podman"

      # --- Model ---
      modelHostDir, # host directory containing the `.ninfer` artifact
      modelFilename, # e.g. "qwen3_8_27b_nvfp4.ninfer"
      modelSha256 ? null, # published SHA-256 of the artifact; null skips verification
      servedModelName, # human-readable name, e.g. "Qwen3.8-27B-NVFP4" (used for the
      # llama-swap key/name/aliases, mirroring the vLLM `vllm:<Name>` convention)
      modelId, # NInfer artifact identity.model_id, e.g. "qwen3.8-27b"
      # (passed as --model-id and used as the OpenAI-facing model id)
      containerName,
      port,

      # --- Engine image ---
      # No registry distribution exists upstream; build locally from a
      # pinned git revision when missing.
      ninferGitRef, # pinned commit/tag of github.com/Neroued/ninfer

      # --- NInfer server tuning knobs (null = use script default) ---
      maxContext ? 252928,
      kvCapacity ? "auto",
      kvDtype ? "int8",
      maxConcurrency ? 8,
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

          # Locally built NInfer image; built from a pinned upstream git
          # revision on first use since there is no registry distribution.
          DOCKER_IMAGE="''${DOCKER_IMAGE:-ninfer:local}"
          NINFER_GIT_REF="''${NINFER_GIT_REF:-${ninferGitRef}}"
          BUILD_IMAGE="''${BUILD_IMAGE:-1}"

          # NInfer server settings.
          MAX_CONTEXT="''${MAX_CONTEXT:-${toString maxContext}}"
          KV_CAPACITY="''${KV_CAPACITY:-${kvCapacity}}"
          KV_DTYPE="''${KV_DTYPE:-${kvDtype}}"
          MAX_CONCURRENCY="''${MAX_CONCURRENCY:-${toString maxConcurrency}}"
          SPEC="''${SPEC-${spec}}"
          DRAFT_TOKENS="''${DRAFT_TOKENS:-${toString draftTokens}}"
          LM_HEAD_DRAFT="''${LM_HEAD_DRAFT:-${if lmHeadDraft then "1" else "0"}}"
          VISION="''${VISION:-${if vision then "1" else "0"}}"

          REMOVE_EXISTING_CONTAINER="''${REMOVE_EXISTING_CONTAINER:-1}"

          # Published SHA-256 of the artifact (model card); empty skips.
          MODEL_SHA256="''${MODEL_SHA256:-${if modelSha256 != null then modelSha256 else ""}}"
          VERIFY_SHA256="''${VERIFY_SHA256:-1}"

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

          if [ "$VERIFY_SHA256" = "1" ] && [ -n "$MODEL_SHA256" ]; then
            echo "Verifying artifact checksum (sha256)..." >&2
            echo "$MODEL_SHA256  $MODEL_HOST_DIR/$MODEL_FILENAME" | sha256sum --check -
          fi

          if ! ${runtimeBin} image exists "$DOCKER_IMAGE"; then
            if [ "$BUILD_IMAGE" != "1" ]; then
              echo "Image $DOCKER_IMAGE not found and BUILD_IMAGE != 1; build it with:" >&2
              echo "  ${runtimeBin} build --pull -t $DOCKER_IMAGE git+https://github.com/Neroued/ninfer.git#''${NINFER_GIT_REF}:" >&2
              exit 1
            fi
            echo "Building image $DOCKER_IMAGE from git+https://github.com/Neroued/ninfer.git#''${NINFER_GIT_REF} (one-time; pulls the CUDA base images and compiles the engine)..." >&2
            ${runtimeBin} build --pull --tag "$DOCKER_IMAGE" "git+https://github.com/Neroued/ninfer.git#''${NINFER_GIT_REF}:"
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
            --kv-dtype "$KV_DTYPE"
            --max-concurrency "$MAX_CONCURRENCY"
            --model-id "${modelId}"
          )

          if [ -n "$SPEC" ]; then
            args+=(--spec "$SPEC" --draft-tokens "$DRAFT_TOKENS")
            if [ "$LM_HEAD_DRAFT" = "1" ]; then
              args+=(--lm-head-draft)
            fi
          fi

          if [ "$VISION" = "1" ]; then
            args+=(--vision)
          fi

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
          echo "  served model id:    ${modelId}"
          echo "  endpoint:           http://localhost:$HOST_PORT/v1"
          echo "  container image:    $DOCKER_IMAGE"
          echo "  max context:        $MAX_CONTEXT (kv: $KV_CAPACITY, $KV_DTYPE)"
          echo "  concurrency:        $MAX_CONCURRENCY (spec: ''${SPEC:-off})"
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
          name = "NInfer ${servedModelName}";
          # The public model id the container accepts (artifact
          # identity.model_id), not the llama-swap key.
          useModelName = modelId;
          # Aliases must be unique across all llama-swap models, so the
          # bare title-case name is intentionally omitted here — it is
          # already claimed by the corresponding vLLM variant's aliases.
          aliases = [
            "ninfer"
            "ninfer:${modelId}"
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
