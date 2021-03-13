#!/usr/bin/env bash
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

echo "remove..."
docker rm --force mediaclient >/dev/null 2>&1

set -e
channels_conf() {
    echo "RUN mkdir -p /etc/mplayer \\"
    cat <<CHANNELS | sed 's/^/ \&\& echo \"/g' | sed 's/$/\" >> \/etc\/mplayer\/channels.conf \\/g'
TELE 5:442000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:411:412:53002
ProSieben:442000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:2201:2203+2202:53621
kabel eins:442000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:2301:2302:53622
RTL II:394000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:1601:1602:53603
VOX:394000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:1701:1702:53604
hr-fernsehen HD:114000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:6551:6552+6553:10355
arte HD:114000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:6661:6662+6663+6669:11120
RTL:122000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_64:1401:1403+1402:53601
SAT.1:122000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_64:2701:2703+2702:53626
3sat HD:626000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:6691:6692+6693:11150
ProSieben MAXX:626000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:6431:6433+6432:53009
zdf_neo HD:642000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:541:542+543+547:11130
RTL NITRO:402000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:981:982:53110
ZDF HD:450000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:6110:6120+6121:11110
ZDF:450000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:110:125+120+121+122:28006
3sat:450000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:210:225+220+221+222:28007
ZDFinfo:450000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:610:625+620+621+622:28011
zdf_neo:450000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:660:675+670+671+672:28014
zdf.kultur:450000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:1110:1125+1120+1121+1122:28016
Bayerisches Fernsehen Süd HD:338000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:5201:5202+5203:10325
Das Erste:338000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:101:106+102+103:28106
Bayerisches FS Süd:338000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:201:206+202+203:28107
Das Erste HD:330000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:5101:5102+5103:11100
ARD-alpha:330000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:1401:1406+1402+1403:28487
tagesschau24:330000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:101:102:28721
arte:330000000:INVERSION_AUTO:6900000:FEC_NONE:QAM_256:401:402+403:28724
CHANNELS
    echo " && mkdir -p ~/.mplayer/ \\"
    echo " && cp /etc/mplayer/channels.conf ~/.mplayer/"
}

entrypoint(){
    echo "RUN echo \"#!/usr/bin/env bash\" > ~/entrypoint.sh \\"
    cat <<ENTRYPOINT | sed 's/^/ \&\& echo \"/g' | sed 's/$/\" >> ~\/entrypoint.sh \\/g'
/opt/bin/mediaclient --start
/opt/bin/mediaclient --mount=192.168.1.35
/opt/bin/mediaclient -e
/opt/bin/mediaclient --setdtvmode=DVBC

# exec gosu $(id -u):$(id -g) smplayer
# exec smplayer
# exec mplayer -aspect 16:9 -demuxer mpegts -vf pp=lb -cache 10239 "dvb://zdf_neo"
exec bash
ENTRYPOINT
    echo " && chmod +x ~/entrypoint.sh"
    echo "ENTRYPOINT ~/entrypoint.sh"
}

GOSU_VERSION='1.7'

_update="apt-get update"
_install="apt-get install -y --no-install-recommends"
_cleanup="eval apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*"
_purge="apt-get purge -y --auto-remove"

echo "build..."
# cat <<EOF
docker build -t mediaclient --rm=true --force-rm=true - <<EOF
FROM ubuntu
MAINTAINER Maximilian Huber <maximilian.huber@tngtech.com>

RUN set -x \
 && $_update && $_install ca-certificates wget curl mplayer smplayer && $_cleanup

RUN set -x \
 && wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-\$(dpkg --print-architecture)" \
 && chmod +x /usr/local/bin/gosu \
 && wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-\$(dpkg --print-architecture).asc" \
 && export GNUPGHOME="\$(mktemp -d)" \
 && gpg --keyserver ha.pool.sks-keyservers.net --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 \
 && gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu \
 && rm -r "\$GNUPGHOME" /usr/local/bin/gosu.asc && $_purge ca-certificates

# Install Sundtek DVB-Driver
RUN set -x \
 && wget http://www.sundtek.de/media/sundtek_netinst.sh \
 && chmod +x sundtek_netinst.sh \
 && ./sundtek_netinst.sh -easyvdr \
 && usermod -a -G video root
 # && ./sundtek_netinst.sh -easyvdr -nolirc -netinst \

$(channels_conf)

$(entrypoint)

ENV DISPLAY :0
EOF

echo "prepare..."
XSOCK=/tmp/.X11-unix
XAUTH=/tmp/.docker.xauth
xauth nlist :0 | sed -e 's/^..../ffff/' | xauth -f $XAUTH nmerge -

echo "run..."
docker run -ti \
       --name=mediaclient \
       -v $XSOCK:$XSOCK -e DISPLAY=$DISPLAY \
       -v $XAUTH:$XAUTH -e XAUTHORITY=$XAUTH \
       -v /dev/snd:/dev/snd \
       mediaclient

