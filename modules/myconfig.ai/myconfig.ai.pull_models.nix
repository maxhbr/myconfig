# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.pull_models — declaratively configure a `pull-models` helper
# script that downloads HuggingFace model repos/files into target directories
# using the `hf` CLI (huggingface-hub).
#
# Model spec format (string):
#   "org/repo"                        – download the entire repo
#   "org/repo/file.ext"               – download a single file (heuristic: '.' in last segment)
#   "org/repo/subdir"                 – download contents of subdir/* (no nesting)
#
# Each entry is laid out as `<target_dir>/<repo_name>[/<file_or_subdir>]`.
{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.myconfig.ai.pull_models;
  modelsJson = pkgs.writeText "pull-models.json" (builtins.toJSON cfg.models);
  pullModels = pkgs.writeShellApplication {
    name = "pull-models";
    runtimeInputs = with pkgs; [
      python3Packages.huggingface-hub
      jq
      coreutils
    ];
    text = ''
      set -euo pipefail
      shopt -s inherit_errexit

      MODELS_JSON="${modelsJson}"
      DRY_RUN=false

      log()   { echo "[INFO]  $*"; }
      warn()  { echo "[WARN]  $*" >&2; }
      err()   { echo "[ERROR] $*" >&2; exit 1; }

      usage() {
          cat <<EOF
      Usage: pull-models [--dry-run|-n] [--models-json <path>] [--help]

        --dry-run, -n         Simulate downloads without writing files.
        --models-json <path>  Override the embedded models JSON file.
        --help, -h            Show this help message.

      Embedded models JSON: $MODELS_JSON
      EOF
          exit 0
      }

      while (( $# )); do
          case "$1" in
              --dry-run|-n)    DRY_RUN=true ;;
              --models-json)   shift; MODELS_JSON="$1" ;;
              -h|--help)       usage ;;
              *)               err "Unknown option: $1" ;;
          esac
          shift
      done

      [[ -f "$MODELS_JSON" ]] || err "File $MODELS_JSON not found"

      download() {
          local repo="$1" include="$2" target_dir="$3"
          target_dir="''${target_dir/#\~/$HOME}"
          mkdir -p "$target_dir"

          if $DRY_RUN; then
              log "[DRY RUN] hf download $repo --include $include --local-dir $target_dir"
              return
          fi

          set -x
          if ! hf download "$repo" --include "$include" --local-dir "$target_dir"; then
              set +x
              err "hf download failed for $repo (include=$include)"
          fi
          set +x
      }

      TOTAL_MODELS=$(jq '[..|arrays|length]|add // 0' "$MODELS_JSON")
      PROCESSED=0

      log "----------------------------------------"

      jq -r 'to_entries[] | .key as $dir | .value[] | "\($dir)\t\(.)"' "$MODELS_JSON" \
      | while IFS=$'\t' read -r dir model; do
          [[ -z "$dir" || -z "$model" ]] && continue
          expanded_dir="''${dir/#\~/$HOME}"
          if [[ ! -d "$expanded_dir" ]]; then
              log "Directory '$expanded_dir' missing – creating"
              if $DRY_RUN; then
                  log "[DRY RUN] mkdir -p $expanded_dir"
              else
                  mkdir -p "$expanded_dir"
              fi
          fi
          dir="$expanded_dir"

          slash_count=$(tr -cd '/' <<<"$model" | wc -c)

          if [[ "$slash_count" -eq 1 ]]; then
              PROCESSED=$((PROCESSED + 1))
              log "[$PROCESSED/$TOTAL_MODELS] $dir – $model (full repo)"
              # Keep org in path: "org/repo" → "$dir/org-repo"
              local_dir="$dir/''${model//\//-}"
              download "$model" "*" "$local_dir"
              [[ -e "$local_dir" ]] && log "$(du -sh "$local_dir" | awk '{print $1}') – $local_dir"
              log "----------------------------------------"
              continue
          fi

          repo_id="''${model%/*}"
          path_in_repo="''${model##*/}"
          # Keep org in path: "org/repo" → "org-repo"
          local_dir="$dir/''${repo_id//\//-}"

          PROCESSED=$((PROCESSED + 1))
          log "[$PROCESSED/$TOTAL_MODELS] $dir – $model"

          if [[ "$path_in_repo" == *.* ]]; then
              target_path="$local_dir/$path_in_repo"
              download "$repo_id" "$path_in_repo" "$local_dir"
          else
              download "$repo_id" "$path_in_repo/*" "$local_dir"
              target_path="$local_dir/$path_in_repo"
          fi
          [[ -e "$target_path" ]] && log "$(du -sh "$target_path" | awk '{print $1}') – $target_path"
          log "----------------------------------------"
      done
    '';
  };
in
{
  options.myconfig.ai.pull_models = with lib; {
    enable = mkEnableOption "myconfig.ai.pull_models (pull-models helper script)";
    models = mkOption {
      type = types.attrsOf (types.listOf types.str);
      default = { };
      example = literalExpression ''
        {
          "/home/mhuber/models" = [
            "unsloth/Qwen3-30B-A3B-GGUF"                                  # full repo → unsloth-Qwen3-30B-A3B-GGUF/
            "unsloth/Qwen3-30B-A3B-GGUF/Qwen3-30B-A3B-UD-Q5_K_XL.gguf"    # single file → unsloth-Qwen3-30B-A3B-GGUF/...
            "unsloth/gemma-3-27B-it-GGUF/BF16"                            # subdir/* → unsloth-gemma-3-27B-it-GGUF/BF16/...
          ];
        }
      '';
      description = ''
        Mapping of target directory → list of HuggingFace model specs to
        pull. Each spec is either `org/repo` (full repo download),
        `org/repo/file.ext` (single file, detected by '.' in the last
        segment), or `org/repo/subdir` (download `subdir/*`). The org
        prefix is kept in the path: `org/repo` becomes `org-repo`.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.sharedModules = [
      {
        home.packages = [ pullModels ];

        systemd.user.services.pull-models = {
          Unit = {
            Description = "Pull HuggingFace models via pull-models script";
            After = [ "default.target" ];
          };
          Service = {
            Type = "oneshot";
            ExecStartPre = "${pkgs.coreutils}/bin/sleep 60";
            ExecStart = "${pullModels}/bin/pull-models";
            RemainAfterExit = true;
          };
          Install = {
            WantedBy = [ "default.target" ];
          };
        };
      }
    ];
  };
}
