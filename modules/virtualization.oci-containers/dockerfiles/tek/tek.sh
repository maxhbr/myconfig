#!/usr/bin/env bash
# Copyright 2016 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

set -x

docker rm --force tek >/dev/null 2>&1

set -e

docker build -t tek --rm=true --force-rm=true - <<EOF
FROM ubuntu:xenial
ENV DEBIAN_FRONTEND noninteractive

ENV _update="apt-get update"
ENV _install="apt-get install -y --no-install-recommends"
ENV _cleanup="eval apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*"
ENV _purge="apt-get purge -y --auto-remove"

RUN set -x \
 && \$_update && \$_install software-properties-common \
 && add-apt-repository ppa:ubuntu-toolchain-r/test \
 && \$_purge software-properties-common && \$_cleanup

RUN set -x \
 && \$_update && \$_install curl wget \
 && wget http://security.ubuntu.com/ubuntu/pool/universe/w/wxwidgets3.0/libwxbase3.0-0_3.0.2-1_amd64.deb \
 && wget http://security.ubuntu.com/ubuntu/pool/universe/w/wxwidgets3.0/libwxgtk3.0-0_3.0.2-1_amd64.deb \
 && wget http://security.ubuntu.com/ubuntu/pool/universe/w/wxwidgets3.0/libwxgtk-webview3.0-0_3.0.2-1_amd64.deb \
 && { dpkg -i libwx*.deb || apt-get install -f -y ; } \
 && \$_cleanup

RUN set -x \
 && curl http://www.trulyergonomic.com/Truly_Ergonomic_Firmware_Upgrade_Linux_v2_0_0.tar.gz | tar xz
WORKDIR /tek-linux
RUN mv 40-tek.rules /etc/udev/rules.d

ENV DISPLAY :0

# ENV LD_LIBRARY_PATH=/usr/include/wx-3.0/wx

CMD ./tek || /bin/bash
EOF
       # libwxbase3.0-0 libwxbase3.0-0-dbg libwxbase3.0-dev \
       # libwxgtk3.0-0 libwxgtk3.0-0-dbg libwxgtk3.0-dev \
       # libwxgtk-media3.0-0 libwxgtk-media3.0-0-dbg libwxgtk-media3.0-dev \


# 10:25:05: Warning: Mismatch between the program and library build versions detected.
# The library used 3.0 (wchar_t,compiler with C++ ABI 1009,wx containers,compatible with 2.8),
# and your program used 3.0 (wchar_t,compiler with C++ ABI 1002,wx containers,compatible with 2.8).
# *** Error in `./tek-linux/tek': malloc(): memory corruption (fast): 0x0000000001441140 ***

XSOCK=/tmp/.X11-unix
XAUTH=/tmp/.docker.xauth
xauth nlist :0 \
    | sed -e 's/^..../ffff/' \
    | xauth -f $XAUTH nmerge -

docker run -ti \
       -v "$XSOCK":"$XSOCK" -e DISPLAY="$DISPLAY" \
       -v "$XAUTH":"$XAUTH" -e XAUTHORITY="$XAUTH" \
       --name=tek \
       tek $1
