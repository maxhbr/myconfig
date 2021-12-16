#!/usr/bin/env bash
set -euo pipefail

release=v135
base="$HOME/.mindustry/$release"
mkdir -p "$base"

client() {
    local jar="$base/Mindustry.jar"
    wget -nc \
        -O "$jar" \
        "https://github.com/Anuken/Mindustry/releases/download/$release/Mindustry.jar" || true

    set -x
    java -Djava.net.preferIPv4Stack=true -jar "$jar" io.anuke.mindustry.desktop.DesktopLauncher
}
server() {
    local jar="$base/server-release.jar"
    wget -nc \
        -O "$jar" \
        "https://github.com/Anuken/Mindustry/releases/download/$release/server-release.jar" || true

    set -x
    java -Djava.net.preferIPv4Stack=true -jar "$jar"
}

${1:-"client"}
