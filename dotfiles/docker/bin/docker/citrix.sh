#!/usr/bin/env bash
# written by: maximilian.huber@tngtech.com
set -e

doBuild=0
TMP=/tmp/citrix_sh
ICACLIENT="icaclient_13.7.0.10276927_amd64.deb"
CFG=receiverconfig.cr
CERTS_DIR=./certs
while getopts "hbi:c:s:" opt; do
    case "$opt" in
    h) echo "usage"
       echo "  initial build (and run):"
       echo "    \$ $0 -b [-i path/to/icaclient_..._amd64.deb] [-c path/to/receiverconfig.cr] [-s path/to/certs]"
       echo "  run:"
       echo "    \$ $0"
       exit 1
        ;;
    b) doBuild=1
        ;;
    i) ICACLIENT=$OPTARG
        ;;
    c) CFG=$OPTARG
       ;;
    s) CERTS_DIR=$OPTARG
       ;;
    esac
done

################################################################################
cd $(dirname $0)
mkdir -p $TMP

docker="$(docker info &> /dev/null || echo "sudo") docker"

dockerfile() {
    dockerfilePath=$TMP/dockerfile.generated
    cat <<EOF > $dockerfilePath
FROM ubuntu:zesty
ENV DEBIAN_FRONTEND noninteractive

ADD $(basename $ICACLIENT) /tmp
RUN set -x \
 && apt-get update -q \
 && dpkg -i /tmp/$(basename $ICACLIENT) || apt-get install -fy -qq

ADD *.crt /opt/Citrix/ICAClient/keystore/cacerts/
RUN /opt/Citrix/ICAClient/util/ctx_rehash

RUN set -x \
 && useradd -ms /bin/bash citrix
USER citrix
WORKDIR /home/citrix

ADD $(basename $CFG) ./
CMD /opt/Citrix/ICAClient/util/new_store $(basename $CFG)
EOF
    echo $dockerfilePath
}

removeContainer() {
    name=$1

    set +e
    $docker rm \
            --force $name \
            >/dev/null 2>&1
    set -e
}

runCitrixWithName() {
    name=$1

    removeContainer $name

    XSOCK=$TMP/.X11-unix
    XAUTH=$TMP/.docker.xauth
    xauth nlist :0 \
        | sed -e 's/^..../ffff/' \
        | xauth -f $XAUTH nmerge -

    $docker run -ti \
            -v $XSOCK:$XSOCK -e DISPLAY=$DISPLAY \
            -v $XAUTH:$XAUTH -e XAUTHORITY=$XAUTH \
            --volume /dev/snd:/dev/snd \
            --volume /tmp/.X11-unix:/tmp/.X11-unix \
            --name=$name \
            --ipc=host \
            citrix
}

build() {
    if [[ ! -f $ICACLIENT ]]; then
        echo "please download the icaclient as debian package"
        exit 1
    fi
    if [[ ! -f $CFG ]]; then
        echo "please get the receiverconfig"
        exit 1
    fi
    if [[ ! -d $CERTS_DIR ]]; then
        echo "please set the folder for certificates"
        exit 1
    fi

    cp -r $CFG $CERTS_DIR/*.crt $ICACLIENT $TMP

    $docker build \
            $TMP \
            -f $(dockerfile) \
            -t citrix \
            --rm=true \
            --force-rm=true

    runCitrixWithName citrix-intermediate
    $docker commit \
            --change "CMD /opt/Citrix/ICAClient/selfservice" \
            $($docker inspect --format="{{.Id}}" citrix-intermediate) \
            citrix
    removeContainer citrix-intermediate
}

################################################################################
[[ $doBuild == 1 ]] && build
runCitrixWithName citrix
