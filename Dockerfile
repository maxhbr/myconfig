# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# see: https://github.com/nix-community/docker-nix
FROM nixorg/nix:latest
MAINTAINER oss@maximilian-huber.de
ARG TERM
ENV TERM=$TERM
ENV NIX_PATH="nixpkgs=channel:nixos-18.09"
RUN printf "#!/usr/bin/env bash\n\$@" > /usr/bin/sudo \
 && chmod +x /usr/bin/sudo
ADD . /root/myconfig
RUN /root/myconfig/rebuild.sh
