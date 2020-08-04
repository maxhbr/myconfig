#!/usr/bin/env nix-shell
#! nix-shell -i bash -p pulseaudio xorg.xmessage
# based on:
# - https://askubuntu.com/a/180661
# - https://askubuntu.com/a/541263

set -e

toggle_telco_mute() {
    toggle_mute "ZOOM VoiceEngine" ||
        toggle_mute "Skype" ||
        echo "nothing to mute was found"
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
            echo "unmuted"
        else
            echo "muted"
        fi
    else
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

popupcmd="echo"
if [[ "$1" == "--popup" ]]; then
    popupcmd="xmessage -timeout 1"
    shift
fi

$popupcmd "$(toggle_telco_mute)"
