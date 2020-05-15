#!/usr/bin/env bash
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

cd "$( dirname "${BASH_SOURCE[0]}" )"

version=$1
url="https://download.jetbrains.com/idea/ideaIU-${version}.tar.gz"

if curl --output /dev/null --silent --head --fail "$url"; then
    sha256="$(nix-prefetch-url --type sha256 $url)"
    echo "{ \"version\": \"$version\", \"sha256\": \"$sha256\" }" | tee ./idea-ultimate.json
else
    echo "url=[$url] does not exist"
fi
