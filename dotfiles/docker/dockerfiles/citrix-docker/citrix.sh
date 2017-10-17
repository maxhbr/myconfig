#!/usr/bin/env bash
# written by: maximilian.huber@tngtech.com
set -e

doBuild=0
TMP=/tmp/citrix_sh
ICACLIENT="icaclient_13.7.0.10276927_amd64.deb"
CFG=receiverconfig.cr
while getopts "hbi:c:" opt; do
    case "$opt" in
    h) echo "usage"
       echo "  initial build (and run):"
       echo "    \$ $0 -b [-i icaclient_..._amd64.deb]"
       echo "  run:"
       echo "    \$ $0"
       exit 1
        ;;
    b) doBuild=1
        ;;
    f) ICACLIENT=$OPTARG
        ;;
    c) CFG=$OPTARG
       ;;
    esac
done

################################################################################
cd $(dirname $0)
mkdir -p $TMP
if [[ ! -f $ICACLIENT ]]; then
    echo "please download the icaclient as debian package"
    exit 1
fi
if [[ ! -f $CFG ]]; then
    echo "please get the receiverconfig and place it here"
    exit 1
fi

docker="$(docker info &> /dev/null || echo "sudo") docker"

dockerfile() {
    dockerfilePath=$TMP/dockerfile.generated
    mkdir -p certs
    cat <<EOF > $dockerfilePath
FROM ubuntu:zesty
ENV DEBIAN_FRONTEND noninteractive

ADD $ICACLIENT /tmp
RUN set -x \
 && apt-get update -q \
 && dpkg -i /tmp/$ICACLIENT || apt-get install -fy -qq

ADD certs/ /opt/Citrix/ICAClient/keystore/cacerts/
RUN /opt/Citrix/ICAClient/util/ctx_rehash

RUN set -x \
 && useradd -ms /bin/bash citrix
USER citrix
WORKDIR /home/citrix

ADD $CFG ./
CMD /opt/Citrix/ICAClient/util/new_store $CFG
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

run() {
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
    cp -r $CFG certs $ICACLIENT $TMP

    $docker build \
            $TMP \
            -f $(dockerfile) \
            -t citrix \
            --rm=true \
            --force-rm=true

    run citrix-intermediate
    $docker commit \
            --change "CMD /opt/Citrix/ICAClient/selfservice" \
            $($docker inspect --format="{{.Id}}" citrix-intermediate) \
            citrix
    removeContainer citrix-intermediate
}

################################################################################
[[ $doBuild == 1 ]] && build
run citrix
