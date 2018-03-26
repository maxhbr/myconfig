#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# based on: https://github.com/fgsch/docker-webex
#    by Federico G. Schwindt <fgsch@lodoss.net>
# and: http://stackoverflow.com/a/25280523
# test it at: http://www.webex.de/test-meeting.html

set -x

docker rm --force webex >/dev/null 2>&1

set -e

installOracleJRE(){
    jrePackage="jre-8u102-linux-i586.tar.gz"
    jreLink="http://download.oracle.com/otn-pub/java/jdk/8u102-b14/$jrePackage"
    jre="jre1.8.0_102"
    cat <<EOF
RUN mkdir /opt/jre && cd /opt/ && \
    wget --header "Cookie: oraclelicense=accept-securebackup-cookie" $jreLink && \
    tar xzf $jrePackage -C /opt/jre/ && \
    update-alternatives --install /usr/bin/java java /opt/jre/$jre/bin/java 100 && \
    update-alternatives --set java /opt/jre/$jre/bin/java && \
    install -d /usr/lib/mozilla/plugins/ && \
    update-alternatives --install /usr/lib/mozilla/plugins/mozilla-javaplugin.so mozilla-javaplugin.so /opt/jre/$jre/lib/i386/libnpjp2.so 1
EOF
}

docker build -t webex --rm=true --force-rm=true - <<EOF
FROM ubuntu:trusty
ENV DEBIAN_FRONTEND noninteractive

RUN dpkg --add-architecture i386 && \
    apt-get update && \
    apt-get install -y wget firefox:i386 libpangox-1.0-0:i386 \
    libpangoxft-1.0-0:i386 libxft2:i386 libxmu6:i386 libxv1:i386 fonts-takao \
    libgcj14-awt:i386

$(installOracleJRE)

RUN useradd -ms /bin/bash webex
USER webex
WORKDIR /home/webex

ENV DISPLAY :0
CMD /usr/bin/firefox --new-instance "http://www.webex.de/test-meeting.html"
EOF

XSOCK=/tmp/.X11-unix
XAUTH=/tmp/.docker.xauth
xauth nlist :0 | sed -e 's/^..../ffff/' | xauth -f $XAUTH nmerge -

docker run -ti \
       -v $XSOCK:$XSOCK -e DISPLAY=$DISPLAY \
       -v $XAUTH:$XAUTH -e XAUTHORITY=$XAUTH \
       --volume /dev/snd:/dev/snd \
       --volume /tmp/.X11-unix:/tmp/.X11-unix \
       --name=webex \
       webex $1
