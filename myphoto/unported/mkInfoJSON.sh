#!/usr/bin/env nix-shell
#! nix-shell -i bash -p dcraw exiftool
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

getDirnameInWorkspace() {
    local file=$(readlink -f "$1")
    if [[ "$file" == *"workspace"* ]]; then
        cat <<EOF
    "dirnameInWorkspace": "$(dirname "$(echo $file | sed 's/.*workspace\///g')")",
EOF
    fi
}

getDataFromRawImage() {
    local output="$(LANG=en_US.UTF-8 dcraw -v -i "$1")"
    cat <<EOF
    "shutter": "$(echo "$output" | grep 'Shutter: ' | cut -d" " -f2)",
    "aperture": "$(echo "$output" | grep 'Aperture: ' | cut -d" " -f2-)",
    "iso": "$(echo "$output" | grep 'ISO speed: ' | cut -d" " -f3-)",
    "focalLength": "$(echo "$output" | grep 'Focal length: ' | cut -d" " -f3-)",
    "camera": "$(echo "$output" | grep 'Camera: ' | cut -d" " -f2-)",
    "timestamp": "$(echo "$output" | grep 'Timestamp: ' | cut -d" " -f2-)",
EOF
}

getDataFromJpgImage() {
    local output="$(LANG=en_US.UTF-8 exiftool -T -createdate -aperture -shutterspeed -iso -focallength -model "$1")"
    while IFS=$'\t' read -r -a list ; do
        cat <<EOF
    "shutter": "${list[2]}",
    "aperture": "${list[1]}",
    "iso": "${list[3]}",
    "focalLength": "${list[4]}",
    "camera":  "${list[5]}",
    "timestamp": "${list[0]}",
EOF
    done <<< "$output"
}

getDataFromImage() {
    local exifImage="$1"
    local extension="$(echo "${exifImage##*.}" | tr '[:upper:]' '[:lower:]')"

    if [[ "$extension" == "arw" ]]; then
        getDataFromRawImage "$exifImage"
    else
        getDataFromJpgImage "$exifImage"
    fi
}

getToolFromImage() {
    local output="$(LANG=en_US.UTF-8 exiftool -T -software "$1")"
    cat<<EOF
    "software": "$output",
EOF
}

main() {
    local fileToDescribe
    local fileToGetDataFrom
    if [[ -f "$1" ]]; then
        fileToDescribe="$1"
    else
        echo "first argument has to be a file"
        exit 1
    fi
    local outputFile="${fileToDescribe}.json"

    if [[ "$2" ]]; then
        if [[ -f "$2" ]]; then
            fileToGetDataFrom="$2"
        else
            echo "second argument, if present, has to be a file"
            exit 1
        fi
    fi

    cat <<EOF>"$outputFile"
{
$(getDirnameInWorkspace "$fileToDescribe")
$(getDataFromImage "${fileToGetDataFrom:-$fileToDescribe}")
$(getToolFromImage "$fileToDescribe")
EOF
    cat <<EOF>>"$outputFile"
    "filename": "$(basename "$fileToDescribe")",
    "title": "",
    "description": ""
}
EOF
}

main $@

