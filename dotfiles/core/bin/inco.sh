#!/usr/bin/env bash

postfix=$(date +%s | sha256sum | base64 | head -c 32 ; echo)
mkdir -p "/tmp/incoChrome_$postfix" && \
    optirun chromium --incognito \
             --user-data-dir="/tmp/incoChrome_$postfix" # &disown
