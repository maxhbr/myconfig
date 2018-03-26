#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# sudo systemctl start tor.service
postfix=$(date +%s | sha256sum | base64 | head -c 32 ; echo)
mkdir -p "/tmp/incoChrome_$postfix" && \
    optirun chromium --incognito \
            --user-data-dir="/tmp/incoChrome_$postfix" \
            $@ &disown
#     --proxy-server="socks://localhost:9050" &disown
