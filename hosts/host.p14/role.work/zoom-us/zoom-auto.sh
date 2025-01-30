#!/usr/bin/env bash

set -euo pipefail

ZOOM_CMD="${ZOOM_CMD:-zoom-us}"
PASTE_CMD="${PASTE_CMD:-wl-paste}"

function convert_zoom_link_to_zoommtg() {
    local zoom_link="$1"
    local confno=$(echo "$zoom_link" | sed -n 's/.*\/j\/\([0-9]*\).*/\1/p')
    if [[ "$zoom_link" =~ .*pwd=.* ]]; then
        local pwd=$(echo "$zoom_link" | sed -n 's/.*pwd=\(.*\)/\1/p')
        echo "zoommtg://zoom.us/join?action=join&confno=$confno&pwd=$pwd"
    else
        echo "zoommtg://zoom.us/join?action=join&confno=$confno"
    fi
}

function open_zoom_link_in_zoom() {
    local zoom_link="$1"
    local zoommtg_link=$(convert_zoom_link_to_zoommtg "$zoom_link")
    echo "... opening zoom link in zoom ..."
    exec "$ZOOM_CMD" "$zoommtg_link"
    exit 0
}

function test_for_zoom_link() {
    local args="$@"
    # bash check that args has no whitespace
    if [[ "$args" =~ ^https://.*zoom.us.*$ && ! "$args" =~ .*[[:space:]].* ]]; then
        echo "... starting zoom ..."
        open_zoom_link_in_zoom "$args"
    else
        echo "... no zoom link found"
    fi
}


function join_from_confno_and_pwd() {
    local confno="$1"
    local pwd="${2:-}"
    local url='zoommtg://zoom.us/join?action=join&confno='"$confno"
    if [[ ! -z "$pwd" ]]; then
      local url="$url"'&pwd='"$pwd"
    fi
    open_zoom_link_in_zoom "$url"
}


if [[ $# -ge 2 ]]; then
    join_from_confno_and_pwd "$@"
elif [[ $# -gt 0 ]]; then
    test_for_zoom_link "$@"
    exit 0
else
  echo "testing for secondary clipboard ..."
  test_for_zoom_link "$($PASTE_CMD || true)"

  echo "testing for primary clipboard ..."
  test_for_zoom_link "$($PASTE_CMD -p || true)"

  echo "zoom-auto did not match anything, just running zoom:"
  exec "$ZOOM_CMD"
fi
