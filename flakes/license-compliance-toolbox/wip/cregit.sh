#!/usr/bin/env bash
# Copyright 2021 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# see: https://github.com/cregit/cregit
set -e

tag=cregit:latest

#################################################################################
# function to build ort docker image
#################################################################################
buildImageIfMissing() {
    if [[ "$(docker images -q $tag 2> /dev/null)" == "" || "$1" == "force" ]]; then
        docker build -t $tag -<<EOF
FROM mozilla/sbt AS builder
RUN set -x \
 && apt-get update && apt-get install -y \
        git \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /cregit
RUN set -x \
 && git clone https://github.com/cregit/cregit /cregit \
 && git clone https://github.com/dmgerman/bfg-repo-cleaner.git --branch blobexec
 # && git clone https://github.com/rtyley/bfg-repo-cleaner

RUN set -x \
 && cd /cregit/bfg-repo-cleaner && sbt bfg/assembly \
 # && cd /cregit/bfg-repo-cleaner/bfg && sbt \
 && cd /cregit/slickGitLog && sbt one-jar \
 && cd /cregit/persons && sbt one-jar \
 && cd /cregit/remapCommits && sbt one-jar

FROM ubuntu:latest
ARG DEBIAN_FRONTEND=noninteractive
RUN set -x \
 && apt-get update && apt-get install -y \
        git wget curl \
        cmake libarchive-dev libxml++2.6-dev libxml2-dev libcurl4-openssl-dev libxslt1-dev libboost-all-dev libantlr-dev libssl-dev libxerces-c-dev exuberant-ctags libdbi-perl libjgit-java libhtml-fromtext-perl libset-scalar-perl libdbd-sqlite3-perl \
        libxerces-c-dev g++ \
 && wget http://131.123.42.38/lmcrs/v1.0.0/srcml_1.0.0-1_ubuntu20.04.deb \
 && dpkg -i srcml_1.0.0-1_ubuntu20.04.deb \
 && rm -rf /var/lib/apt/lists/*

RUN set -x \
 && git clone https://github.com/universal-ctags/ctags \
 && cd ctags \
 && ./autogen.sh \
 && ./configure \
 && make \
 && make install \
 && cd .. \
 && rm -rf ctags

COPY --from=builder /cregit/ /cregit/
WORKDIR /cregit

RUN set -x \
 && cd /cregit/tokenize/srcMLtoken \
 && make

RUN set -x \
 && echo "#!/usr/bin/env bash" > /entrypoint.sh \
 && echo "set -euxo pipefail" >> /entrypoint.sh \
 && echo "export BFG_MEMO_DIR=/tmp/memo" >> /entrypoint.sh \
 && echo "export BFG_TOKENIZE_CMD='/home/dmg/git.dmg/cregit-scala/tokenize/tokenizeSrcMl.pl --srcml2token=/home/dmg/git.dmg/cregit-scala/tokenize/srcMLtoken/srcml2token --srcml=srcml --ctags=/usr/local/bin/ctags'" >> /entrypoint.sh \
 && echo "echo start..." >> /entrypoint.sh \
 && echo "java -jar bfg-cregit.jar '--blob-exec:/cregit/tokenizeByBlobId/tokenBySha.pl=.[ch]$' --no-blob-protection /workdir" >> /entrypoint.sh \
 && chmod +x /entrypoint.sh
ENTRYPOINT /entrypoint.sh
EOF
    else
        echo "docker image already build"
    fi
}

computeOutFolder() {
    local workdir="$(readlink -f "$1")"
    echo "${workdir%_ort}_cregit"
}

getOutFolder() {
    local out="$(computeOutFolder "$@")"
    mkdir -p "$out"
    echo "$out"
}

runCregit() {
    local workdir="$(readlink -f "$1")"
    [[ ! -d "$workdir" ]] && exit 1
    shift

    (set -x;
     docker run -i \
            --rm \
            -v /etc/group:/etc/group:ro -v /etc/passwd:/etc/passwd:ro -u $(id -u $USER):$(id -g $USER) \
            -v "$workdir:/workdir" \
            -v "$(getOutFolder "$workdir")":/out \
            -w /workdir \
            --net=host \
            $tag \
            $@;
     times
     )
}

#################################################################################
# main
#################################################################################
buildImageIfMissing force
runCregit "$1"
