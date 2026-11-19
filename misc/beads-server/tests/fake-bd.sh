#!/usr/bin/env bash
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# fake-bd: emit canned bd output so the unittest suites need no Dolt.
#
# Drop-in `bd` replacement. The state lives in a plain JSON array at
# $BEADS_DIR/issues.json (or <cwd>/.beads/issues.json), which tests
# create with a heredoc. Supports:
#   list --all --limit 0 --json
#   show <id...> --json [--include-comments]
#   count --json
#   update <id> [--status S] [--priority P] [--assignee A] [--title T]
#          [--body-file -] [--if-status S]
#   comment <id> --stdin
# Everything else exits 1 so untested paths fail loudly.

set -euo pipefail

beads_dir=${BEADS_DIR:-$PWD/.beads}
db="$beads_dir/issues.json"

cmd=$1
shift || true

# apply FIELD VALUE to issue ID in $db — one jq pass per field, so
# values travel as --arg/--argjson, never spliced into a program string
apply() {
    local id=$1 field=$2 kind=$3 value=$4
    local tmp
    tmp=$(mktemp)
    if [ "$kind" = json ]; then
        jq --arg id "$id" --arg field "$field" --argjson value "$value" \
            'map(if .id == $id then .[$field] = $value
           | .updated_at = "2026-01-01T00:00:00Z" else . end)' "$db" >"$tmp"
    else
        jq --arg id "$id" --arg field "$field" --arg value "$value" \
            'map(if .id == $id then .[$field] = $value
           | .updated_at = "2026-01-01T00:00:00Z" else . end)' "$db" >"$tmp"
    fi
    mv "$tmp" "$db"
}

case $cmd in
    list)
        cat "$db"
        ;;
    count)
        jq '{count: length, schema_version: 1}' "$db"
        ;;
    show)
        ids=()
        for arg in "$@"; do
            case $arg in
                --json | --include-comments) ;;
                *) ids+=("$arg") ;;
            esac
        done
        jq --argjson ids "$(printf '%s\n' "${ids[@]}" | jq -R . | jq -s .)" \
            'map(select(.id as $i | $ids | index($i)))' "$db"
        ;;
    update)
        id=$1
        shift
        if_status=""
        nsets=0
        while [ $# -gt 0 ]; do
            case $1 in
                --status)
                    apply "$id" status text "$2"
                    nsets=$((nsets + 1))
                    shift 2
                    ;;
                --priority)
                    apply "$id" priority json "$2"
                    nsets=$((nsets + 1))
                    shift 2
                    ;;
                --assignee)
                    apply "$id" assignee text "$2"
                    nsets=$((nsets + 1))
                    shift 2
                    ;;
                --title)
                    apply "$id" title text "$2"
                    nsets=$((nsets + 1))
                    shift 2
                    ;;
                --body-file)
                    body=$(cat)
                    apply "$id" description text "$body"
                    nsets=$((nsets + 1))
                    shift
                    if [ "${1:-}" = "-" ]; then shift; fi
                    ;;
                --if-status)
                    if_status=$2
                    shift 2
                    ;;
                *)
                    shift
                    ;;
            esac
        done
        if [ -n "$if_status" ]; then
            current=$(jq -r --arg id "$id" \
                'map(select(.id == $id))[0].status // "none"' "$db")
            if [ "$current" != "$if_status" ]; then
                echo "status mismatch: $id has status \"$current\", expected \"$if_status\"" >&2
                exit 13
            fi
        fi
        if [ "$nsets" -eq 0 ]; then
            echo "no updates specified" >&2
            exit 1
        fi
        echo "✓ Updated issue: $id"
        ;;
    comment)
        id=$1
        shift
        text=$(cat)
        tmp=$(mktemp)
        jq --arg id "$id" --arg text "$text" \
            'map(if .id == $id then .comment_count = (.comment_count // 0) + 1
           else . end)' "$db" >"$tmp"
        mv "$tmp" "$db"
        echo "✓ Comment added to $id"
        ;;
    *)
        echo "fake-bd: unsupported command: $cmd $*" >&2
        exit 1
        ;;
esac
