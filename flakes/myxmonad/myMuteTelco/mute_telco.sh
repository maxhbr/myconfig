#!/usr/bin/env bash
# based on:
# - https://askubuntu.com/a/180661
# - https://askubuntu.com/a/541263

# TODO:
# - Bail out, if uncertain

set -e

lastStateFile="/tmp/myMute-lastState"

toggle_telco_mute() {
    toggle_mute "ZOOM VoiceEngine" ||
        toggle_mute "Skype" ||
        {
            echo "nothing to mute was found"
            rm "/tmp/myMute-lastState" || true
        }
}

toggle_mute() {
    local name="$1"

    local index=$(get_index "$name")
    if [[ "$index" ]]; then
        local state_file="$(get_state_file "$name" "$index")"

        local other_state="$(get_other_state_from_file "$state_file")"
        pacmd set-source-output-mute "$index" "$other_state"
        echo $other_state > "$state_file"
        if [[ "$other_state" == "0" ]]; then
            echo "unmute $(echo $name | awk '{print $1;}')"
            rm "/tmp/myMute-lastState" || true
            blink1-tool --rgb '#770000' &>/dev/null || true &
        else
            echo "mute $(echo $name | awk '{print $1;}')" | tee "$lastStateFile"
            blink1-tool --rgb '#007700' &>/dev/null || true &
        fi
    else
        blink1-tool --rgb 0xff,0xff,0x00 --blink 1 &>/dev/null || true &
        return 1
    fi
}

get_index() {
    local name="$1"

    pacmd list-source-outputs |
        awk -v name="$name" '
            $1 == "index:" {idx = $2}
            /.*application.name = "'"$name"'"/ {print idx; exit}
        '
}

get_state_file() {
    local name="$1"
    local index="$2"

    echo "/tmp/myMute-$(echo $name | base64)-$index"
}

get_other_state_from_file() {
    local state_file="$1"

    if [[ -f "$state_file" ]]; then
        [[ "$(cat "$state_file" | cut -c1-1)" == "0" ]] && echo "1" || echo "0"
    else
        echo "1"
    fi
}

print_mute_status() {
    local prefix="$1"
    local postfix="$2"
    local alternative="$3"
    if [[ -f "$lastStateFile" ]]; then
        printf "$prefix$(cat $lastStateFile)$postfix"
    else
        printf "$alternative"
    fi
}

if [[ "$1" == "--print-status" ]]; then
    shift
    print_mute_status "$@"
else
    toggle_telco_mute
fi

