#!/usr/bin/env nix-shell
#! nix-shell -i bash -p python313Packages.huggingface-hub jq

set -euo pipefail
shopt -s inherit_errexit

# ----------------------------------------------------------------------
# Configuration
# ----------------------------------------------------------------------
MODELS_JSON="models.json"
DRY_RUN=false

# ----------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------
log()    { echo "[INFO]  $*"; }
warn()   { echo "[WARN]  $*" >&2; }
error()  { echo "[ERROR] $*" >&2; exit 1; }

usage() {
    cat <<'EOF'
Usage: $(basename "$0") [--dry-run|-n] [--debug] [--help]

  --dry-run, -n   Simulate downloads without writing files.
  --debug         Show the hf download command (set -x) and extra info.
  --help          Show this help message.
EOF
    exit 0
}

parse_args() {
    while (( "$#" )); do
        case "$1" in
            --dry-run|-n) DRY_RUN=true ;;
            -h|--help)    usage ;;
            *)            error "Unknown option: $1" ;;
        esac
        shift
done
}

# Ensure destination directory exists before download
download() {
    local repo="$1" include="$2" target_dir="$3"
    # Expand ~ to home directory
    target_dir="${target_dir/#\~/$HOME}"
    mkdir -p "$target_dir"

    if $DRY_RUN; then
        log "[DRY RUN] hf download $repo --include $include --local-dir $target_dir"
        return
    fi

    set -x
    if ! hf download "$repo" --include "$include" --local-dir "$target_dir"; then
        set +x
        error "hf download failed for $repo (include=$include)"
    fi
    set +x
}

# ----------------------------------------------------------------------
# Main
# ----------------------------------------------------------------------
parse_args "$@"

[[ -f "$MODELS_JSON" ]] || error "File $MODELS_JSON not found"

# Count total number of model entries (sum of array lengths)
TOTAL_MODELS=$(jq '[..|arrays|length]|add' "$MODELS_JSON")
PROCESSED=0

log "----------------------------------------"

jq -r 'to_entries[] | .key as $dir | .value[] | "\($dir)\t\(.)"' "$MODELS_JSON" |
while IFS=$'\t' read -r dir model; do
    [[ -z "$dir" || -z "$model" ]] && continue
    [[ -d "$dir" ]] || { warn "Directory '$dir' missing – skipping"; continue; }

    # Count slashes to determine format: org/repo = full download, org/repo/path = partial
    slash_count=$(echo "$model" | tr -cd '/' | wc -c)
    
    if [[ "$slash_count" -eq 1 ]]; then
        # Full repo download (org/repo format)
        PROCESSED=$((PROCESSED + 1))
        log "[$PROCESSED/$TOTAL_MODELS] $dir – $model (full repo)"
        local_dir="$dir/${model##*/}"
        download "$model" "*" "$local_dir"
        [[ -e "$local_dir" ]] && log "$(du -sh "$local_dir" | awk '{print $1}') – $local_dir"
        log "----------------------------------------"
        continue
    fi
    
    repo_id="${model%/*}"
    path_in_repo="${model##*/}"
    repo_name="${repo_id##*/}"
    local_dir="$dir/$repo_name"

    PROCESSED=$((PROCESSED + 1))
    log "[$PROCESSED/$TOTAL_MODELS] $dir – $model"

    # Simple heuristic: treat as file if last component contains a dot
    if [[ "$path_in_repo" == *.* ]]; then
        target_path="$local_dir/$path_in_repo"
        [[ -f "$target_path" ]] || download "$repo_id" "$path_in_repo" "$local_dir"
    else
        # For folders, download contents to local_dir (not nested)
        [[ -d "$local_dir/$path_in_repo" ]] || download "$repo_id" "$path_in_repo/*" "$local_dir"
        target_path="$local_dir/$path_in_repo"
    fi
    [[ -e "$target_path" ]] && log "$(du -sh "$target_path" | awk '{print $1}') – $target_path"
    log "----------------------------------------"
done

exit 0
