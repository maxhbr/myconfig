#!/usr/bin/env bash
cd "$(dirname $0)"
java -Djava.net.preferIPv4Stack=true -jar jre/desktop.jar io.anuke.mindustry.desktop.DesktopLauncher
