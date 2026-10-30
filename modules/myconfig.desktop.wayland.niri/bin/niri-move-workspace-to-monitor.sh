#!/usr/bin/env bash
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Move the focused workspace to the monitor left/right of the focused
# monitor, WITHOUT following the focus there: niri's
# move-workspace-to-monitor-* always activates the moved workspace on
# the target monitor (focus and cursor follow — no flag exists for it
# in the pinned niri), so this script restores focus to the origin
# monitor afterwards (focus-monitor by name).
#
# A no-op on the edge, exactly like niri's own action when no neighbor
# exists: the neighbor is computed with niri's own output_left_of/
# output_right_of rule (center-x strictly left/right, geometry
# overlapping the focused output's horizontal band, nearest wins), so
# "no neighbor" here means "no neighbor" for niri too.
#
#   niri-move-workspace-to-monitor <left|right>
set -euo pipefail

if [[ $# -ne 1 || $1 != left && $1 != right ]]; then
    echo "usage: ${0##*/} <left|right>" >&2
    exit 2
fi
dir="$1"

origin="$(
    niri msg --json focused-output |
        jq -r '.name // empty'
)"
[[ -z "$origin" ]] && exit 0

target="$(
    niri msg --json outputs |
        jq -r --arg origin "$origin" --arg dir "$dir" '
          def cx($o): $o.x + $o.width / 2;
          .[$origin].logical as $ol
          | [ to_entries[]
              | select(.value.logical != null)
              | { name: .key, g: .value.logical } ]
          | ( if $dir == "left" then
                map(select(
                  cx(.g) < cx($ol)
                  and .g.y + .g.height > $ol.y
                  and .g.y < $ol.y + $ol.height))
                | sort_by(cx($ol) - cx(.g))
              else
                map(select(
                  cx(.g) > cx($ol)
                  and .g.y + .g.height > $ol.y
                  and .g.y < $ol.y + $ol.height))
                | sort_by(cx(.g) - cx($ol))
              end )
          | .[0].name // empty
        '
)"
[[ -z "$target" ]] && exit 0

niri msg action "move-workspace-to-monitor-$dir"
niri msg action focus-monitor "$origin"
