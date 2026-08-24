# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# Common NInfer container launch-script factory (Podman or Docker).
#
# NInfer (https://github.com/Neroued/ninfer) is a from-scratch C++/CUDA
# inference engine for a closed set of registered Qwen checkpoints.
# Unlike vLLM there is no public registry image — the engine ships only
# a Dockerfile (see https://github.com/Neroued/ninfer/blob/master/Dockerfile
# and the "Docker" section of the README), so the runtime image is built
# once from a pinned upstream git revision into the local container store
# and tagged `ninfer:local` by default. When that image is missing, the
# launch script itself performs the build, mirroring the upstream README
# (`<runtime> build --tag ninfer:local .` in a clone of the repo at the
# pinned revision) so the variant is turnkey rather than requiring an
# out-of-band build step.
#
# The Dockerfile's unqualified `FROM nvidia/cuda:...` base image still
# needs a default registry to resolve. Podman (c/image) takes that from
# `unqualified-search-registries` in registries.conf, and a bare NixOS
# host ships no such file, so the build passes a generated one-liner
# (`unqualified-search-registries = ["docker.io"]`) through the
# CONTAINERS_REGISTRIES_CONF environment variable instead of touching
# the host. Docker ignores the variable and always defaults to
# Docker Hub anyway.
#
# Model-path constraint (host.thing): the model subvolumes live on a
# Btrfs volume that is exposed *twice* — writable at
# /home/mhuber/models (the download location) and read-only at /models
# (the serving location). The factory therefore takes both paths
# explicitly:
#   modelDownloadDir – writable; the launcher's `hf download` writes here
#   modelHostDir     – read-only; existence checks, checksum verification,
#                      and the container bind mount (`/models:ro`) read here
# Downloads are never pointed at the read-only path, and no path
# rewriting happens inside the script — the variant decides which path
# is which.
#
# Each `.ninfer` artifact is a single file (not a directory checkout
# like vLLM's Hugging Face repos), so only the one required file is
# downloaded. When the model card publishes a SHA-256 for the artifact,
# pass it as `modelSha256` so the script verifies it before every
# launch.
#
# llama-swap aliases must be unique across *all* models (the config
# loader hard-fails on duplicates). The factory only claims
# `ninfer:<modelId>`, which is unique per artifact by construction; the
# human-friendly bare `ninfer` alias is claimed by the primary variant
# via `extraConfig.aliases` (see ./docker.ninfer.cuda.nix).

{ pkgs }:

let
  mkNinferDockerized =
    {
      containerRuntime ? "podman", # "docker" or "podman"

      # --- Model artifact ---
      modelDownloadDir, # writable host dir for `hf download` (e.g. /home/mhuber/models/...)
      modelHostDir, # read-only host dir for checks + the /models bind mount (e.g. /models/...)
      modelFilename, # e.g. "qwen3_8_27b_nvfp4.ninfer"
      modelSha256 ? null, # published SHA-256 of the artifact; null skips verification
      modelHfRepo ? null, # Hugging Face repo for on-demand download; null skips it
      # The artifact's identity.model_id — the public OpenAI-facing model
      # id. Passed explicitly as --model-id instead of relying on the
      # artifact default, and used as the llama-swap `useModelName`.
      modelId,
      # Display name for the llama-swap entry; the model key is
      # "ninfer:<name>".
      servedModelName,
      containerName,
      port,

      # --- Engine image ---
      # No registry distribution exists upstream; the image is built
      # locally from the pinned git revision when missing.
      ninferImage ? "ninfer:local",
      ninferGitRef, # pinned commit/tag of github.com/Neroued/ninfer

      # --- NInfer server tuning (defaults = published RTX 5090 profile) ---
      maxContext ? 252928,
      kvCapacity ? "auto",
      kvDtype ? "int8",
      maxConcurrency ? 8,
      # Speculative decoding backend; "" disables it by default.
      spec ? "mtp",
      draftTokens ? 3,
      lmHeadDraft ? true,
      vision ? false,
      noCudaGraph ? false,

      # Extra llama-swap model settings (e.g. the bare "ninfer" alias).
      extraConfig ? { },
    }:
    let
      runtime = pkgs.${containerRuntime};
      runtimeBin = "${runtime}/bin/${containerRuntime}";
      # Pre-computed for the string interpolation below, which does not
      # accept `or` on bare variables.
      modelSha256Str = if modelSha256 == null then "" else modelSha256;
      modelHfRepoStr = if modelHfRepo == null then "" else modelHfRepo;

      ninferPkg = pkgs.writeShellApplication {
        name = containerName;

        runtimeInputs = [
          pkgs.coreutils
          # First launch may build the image from a clone of the pinned
          # git revision, which requires git.
          pkgs.git
          runtime
          (pkgs.python3.withPackages (ps: [ ps.huggingface-hub ]))
        ];

        # The script body uses the repo shell convention (shfmt -i 4).
        # The closing '' sits on the final line so the string has no
        # trailing newline; writeShellApplication appends exactly one,
        # which keeps the generated script clean under `shfmt -d -s -ci`.
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

          # Model paths: downloads go through the writable directory;
          # existence checks and the container bind mount use the
          # read-only serving directory.
          MODEL_DOWNLOAD_DIR="''${MODEL_DOWNLOAD_DIR:-${modelDownloadDir}}"
          MODEL_HOST_DIR="''${MODEL_HOST_DIR:-${modelHostDir}}"
          # Registered artifact inside those directories.
          MODEL_FILENAME="''${MODEL_FILENAME:-${modelFilename}}"

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
          MAX_CONTEXT="''${MAX_CONTEXT:-${toString maxContext}}"
          KV_CAPACITY="''${KV_CAPACITY:-${kvCapacity}}"
          MAX_CONCURRENCY="''${MAX_CONCURRENCY:-${toString maxConcurrency}}"
          KV_DTYPE="''${KV_DTYPE:-${kvDtype}}"
          # Speculative decoding backend; set SPEC="" to disable.
          SPEC="''${SPEC-${spec}}"
          DRAFT_TOKENS="''${DRAFT_TOKENS:-${toString draftTokens}}"
          LM_HEAD_DRAFT="''${LM_HEAD_DRAFT:-${if lmHeadDraft then "1" else "0"}}"
          # Vision is disabled by default (like the text-only vLLM variants);
          # the artifact supports it, enable with VISION=1.
          VISION="''${VISION:-${if vision then "1" else "0"}}"
          NO_CUDA_GRAPH="''${NO_CUDA_GRAPH:-${if noCudaGraph then "1" else "0"}}"
          REMOVE_EXISTING_CONTAINER="''${REMOVE_EXISTING_CONTAINER:-1}"

          # Published SHA-256 of the artifact (model card); empty skips.
          MODEL_SHA256="''${MODEL_SHA256:-${modelSha256Str}}"
          VERIFY_SHA256="''${VERIFY_SHA256:-1}"

          MODEL_HF_REPO="''${MODEL_HF_REPO:-${modelHfRepoStr}}"

          if [ -n "$MODEL_HF_REPO" ]; then
              echo "Ensuring model is present: $MODEL_HF_REPO/$MODEL_FILENAME" >&2
              # Only the single required artifact, and only through the
              # writable download directory (never the read-only mount).
              hf download "$MODEL_HF_REPO" "$MODEL_FILENAME" --local-dir "$MODEL_DOWNLOAD_DIR"
          fi

          if [ ! -d "$MODEL_HOST_DIR" ]; then
              echo "Model directory does not exist: $MODEL_HOST_DIR" >&2
              exit 1
          fi

          if [ ! -f "$MODEL_HOST_DIR/$MODEL_FILENAME" ]; then
              echo "Model artifact does not exist: $MODEL_HOST_DIR/$MODEL_FILENAME" >&2
              exit 1
          fi

          if [ "$VERIFY_SHA256" = "1" ] && [ -n "$MODEL_SHA256" ]; then
              echo "Verifying artifact checksum (sha256)..." >&2
              echo "$MODEL_SHA256  $MODEL_HOST_DIR/$MODEL_FILENAME" | sha256sum --check -
          fi

          if ! ${runtimeBin} image exists "$NINFER_IMAGE"; then
              if [ "$BUILD_IMAGE" != "1" ]; then
                  echo "Image $NINFER_IMAGE not found and BUILD_IMAGE != 1; build it from a local clone:" >&2
                  echo "  git clone https://github.com/Neroued/ninfer.git && cd ninfer && git checkout ''${NINFER_GIT_REF}" >&2
                  echo "  (podman: additionally point CONTAINERS_REGISTRIES_CONF at a file with 'unqualified-search-registries = [\"docker.io\"]')" >&2
                  echo "  ${runtimeBin} build --pull -t $NINFER_IMAGE ." >&2
                  exit 1
              fi
              # No registry distribution exists upstream and neither
              # podman nor docker accept a git URL as build context; the
              # README builds from a local clone, so clone the pinned
              # revision into a temp dir and build from that tree.
              echo "Building image $NINFER_IMAGE from github.com/Neroued/ninfer @ ''${NINFER_GIT_REF} (one-time; pulls the CUDA base images and compiles the engine)..." >&2
              NINFER_BUILD_DIR="$(mktemp -d)"
              trap 'rm -rf "$NINFER_BUILD_DIR"' EXIT
              git clone --quiet https://github.com/Neroued/ninfer.git "$NINFER_BUILD_DIR/ninfer"
              git -C "$NINFER_BUILD_DIR/ninfer" checkout --quiet "$NINFER_GIT_REF"
              # Provide Podman a default registry for the unqualified
              # `FROM` base image (host NixOS ships no registries.conf).
              printf 'unqualified-search-registries = ["docker.io"]\n' >"$NINFER_BUILD_DIR/registries.conf"
              CONTAINERS_REGISTRIES_CONF="$NINFER_BUILD_DIR/registries.conf" ${runtimeBin} build --pull --tag "$NINFER_IMAGE" "$NINFER_BUILD_DIR/ninfer"
              rm -rf "$NINFER_BUILD_DIR"
              trap - EXIT
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
              "$NINFER_IMAGE"
              ninfer-serve "/models/$MODEL_FILENAME"
              --host 0.0.0.0
              --port 8080
              --model-id "${modelId}"
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

          if [ -n "''${EXTRA_NINFER_ARGS:-}" ]; then
              # shellcheck disable=SC2206
              extra_args=($EXTRA_NINFER_ARGS)
              args+=("''${extra_args[@]}")
          fi

          echo "Starting NInfer container:"
          echo "  model:              $MODEL_HOST_DIR/$MODEL_FILENAME"
          echo "  served model id:    ${modelId}"
          echo "  endpoint:           http://localhost:$HOST_PORT/v1"
          echo "  image:              $NINFER_IMAGE"
          echo "  max context:        $MAX_CONTEXT (kv: $KV_CAPACITY, $KV_DTYPE)"
          echo "  concurrency:        $MAX_CONCURRENCY (spec: ''${SPEC:-off})"
          echo

          set -x
          exec "''${args[@]}"'';
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
          # See the file header: only the artifact-unique alias is
          # claimed here; the bare "ninfer" alias is claimed by the
          # primary variant via extraConfig.aliases.
          aliases = [ "ninfer:${modelId}" ] ++ (extraConfig.aliases or [ ]);
          cmdStop = "${runtimeBin} stop ${containerName}";
          ttl = 0;
        };
      };
    };
in
{
  inherit mkNinferDockerized;
}
