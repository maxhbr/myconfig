#!/usr/bin/env bash

set -e

tag=bang:latest
mytag=mybang:latest

help() {
    cat <<EOF
$ $0 tbd.
EOF
}

forceRebuild() {
    docker rmi $mytag || true
    docker rmi $tag || true
}

buildImageIfMissing() {
    if [[ "$(docker images -q $mytag 2> /dev/null)" == "" ]]; then
        if [[ "$(docker images -q $tag 2> /dev/null)" == "" ]]; then
            BANG=$(mktemp -d)
            trap 'rm -rf $BANG' EXIT
            git clone https://github.com/armijnhemel/binaryanalysis-ng $BANG

            docker build -t $tag $BANG
            rm -rf $BANG
        else
            echo "docker image $tag already build"
        fi

        MYBANG=$(mktemp -d)
        trap 'rm -rf $MYBANG' EXIT
        cat <<EOF >"$MYBANG/bang.config"
[configuration]
baseunpackdirectory = /out/tmp
temporarydirectory = /out/tmp
threads            = 0
#removescandirectory = no
#logging = yes
#tlshmaximum = 31457280
#bytecounter = no
json = no
runfilescans = yes

[database]
#postgresql_enabled = yes
postgresql_user     = bang
postgresql_password = bang
postgresql_db       = bang
postgresql_error_fatal = no
#postgresql_host = 127.0.0.1
#postgresql_port = 5432

[elasticsearch]
elastic_enabled = no
elastic_user = bang
elastic_password = bangbang
elastic_index = bang
#elastic_connectionerrorfatal = yes
#elastic_host = 127.0.0.1
#elastic_port = 9200
EOF
        cat <<EOF >"$MYBANG/entrypoint.sh"
#!/usr/bin/env bash
set -e
input=/in
mkdir -p /out/tmp
find "\$input" \
    -type f \
    -print \
    -exec python3 bang-scanner \
        -c /bang.config \
        -f "{}" \;
python3 bangshell
EOF
        cat <<EOF >"$MYBANG/Dockerfile"
FROM $tag
# TODO: postgres
# RUN set -x \
#  && dnf update -y \
#  && dnf install -y postgresql-server \
#                    postgresql-contrib \
#  && postgresql-setup initdb
# TODO: eclasticsearch
COPY bang.config /
COPY entrypoint.sh /
RUN set -x \
 && chmod +x /entrypoint.sh
ENTRYPOINT /entrypoint.sh
EOF
        docker build -t "$mytag" "$MYBANG"
    else
        echo "docker image $mytag already build"
    fi
}

getOutFolder() {
    local in="$(readlink -f "$1")"
    local out="${in%_bang}_bang"
    mkdir -p "$out"
    echo "$out"
}

runBangShell() {
    local out="$(readlink -f "$1")"
    [[ ! -d "$out" ]] && exit 1
    shift
    (set -x;
     docker run -i \
            --rm \
            -u $(id -u $USER):$(id -g $USER) \
            -v "$out":/out  \
            --net=host \
            $tag;
     times
    )
}

runBang() {
    local in="$(readlink -f "$1")"
    [[ ! -d "$in" ]] && exit 1
    bn="$(basename "$in")"
    shift
    (set -x;
     docker run -i \
            --rm \
            -u $(id -u $USER):$(id -g $USER) \
            -v "$in":/in -v "$(getOutFolder "$in")":/out \
            --net=host \
            $mytag;
     times
    )
}

#################################################################################
# main
#################################################################################
if [[ "$1" == "-fr" ]]; then
    shift
    forceRebuild
fi
buildImageIfMissing

if [[ "$1" == "-shell" ]]; then
    shift
    runBangShell "$@"
else
    runBang "$@"
fi

