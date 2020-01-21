#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# src: https://github.com/src-d/go-license-detector
# blog: https://blog.sourced.tech/post/gld/
#
# to run:
# $ $0 build
# $ $0 https://github.com/src-d/go-git

set -e
docker="$(docker info &> /dev/null || echo "sudo") docker"

build() {
    $docker build -t go-license-detector --rm=true --force-rm=true - <<'EOF'
FROM golang:1.8
RUN set -x \
 && go get -v gopkg.in/src-d/go-license-detector.v2/...
RUN set -x \
 && echo '#!/usr/bin/env bash' >/entrypoint.sh \
 && echo 'exec /go/bin/license-detector $@' >>/entrypoint.sh \
 && chmod +x /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
CMD ["--help"]
EOF
}

run() {
    $docker rm \
            --force go-license-detector \
            >/dev/null 2>&1 || true

    if [[ -d "$1" ]]; then
        workdir=$(readlink -f "$1")
        $docker run \
                --name=go-license-detector \
                -v "$workdir:/toScan" \
                go-license-detector /toScan
    else
        $docker run \
                --name=go-license-detector \
                go-license-detector $@
    fi
}

################################################################################
if [[ "$1" == "build" ]]; then
    build
else
    run $@
fi
