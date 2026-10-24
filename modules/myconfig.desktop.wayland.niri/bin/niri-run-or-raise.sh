#!/usr/bin/env bash
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Run-or-raise for niri: focus an application's window if there is one, spawn
# it otherwise.
#
#   niri-run-or-raise <app-id> [command...]
#
# `<app-id>` is matched case-insensitively as a substring of the windows'
# `app_id`, so `thunderbird` also matches ids like `thunderbird-beta` and
# `pavucontrol` matches `org.pulseaudio.pavucontrol`.
# `command...` defaults to the app id and is spawned through niri when no
# window matches.
#
# Windows, their ids and focus state come from `niri msg --json windows`, so
# the script never parses niri's human-readable output (the JSON endpoint is
# the stable interface; see https://github.com/niri-wm/niri/discussions/1096).
#
# Behaviour:
# - no matching window: spawn the command.
# - matching window, none focused: focus the most recently used match.
# - matching window already focused: focus the next match, so repeated
#   presses cycle through all of the application's windows.
set -euo pipefail

if [[ $# -lt 1 ]]; then
    echo "usage: ${0##*/} <app-id> [command...]" >&2
    exit 2
fi

app="$1"
shift
if [[ $# -eq 0 ]]; then
    set -- "$app"
fi

# Line 1: id of the focused match (empty if no match is focused).
# Line 2..n: ids of all matches, most recently used first.
mapfile -t matches < <(
    niri msg --json windows | jq -r --arg app "$app" '
        ($app | ascii_downcase) as $needle
        | [.[] | select((.app_id // "") | ascii_downcase | contains($needle))]
        | sort_by(.focus_timestamp // { secs: 0, nanos: 0 } | [.secs, .nanos])
        | reverse
        | (map(select(.is_focused)) | .[0].id // ""),
          (.[] | .id)
    '
)

focused="${matches[0]:-}"
ids=()
if [[ ${#matches[@]} -gt 1 ]]; then
    ids=("${matches[@]:1}")
fi

if [[ ${#ids[@]} -eq 0 ]]; then
    niri msg action spawn -- "$@"
    exit 0
fi

target="${ids[0]}"
if [[ -n $focused ]]; then
    for i in "${!ids[@]}"; do
        if [[ ${ids[$i]} == "$focused" ]]; then
            target="${ids[$(((i + 1) % ${#ids[@]}))]}"
            break
        fi
    done
fi

niri msg action focus-window --id "$target"
