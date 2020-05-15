#!/usr/bin/env bash

set -e

shelltag=bang:latest
runnertag=bangrunner:latest
postgrestag=bangpostgres:latest

help() {
    cat <<EOF
$ $0 tbd.
EOF
}

forceRebuild() {
    docker rmi $runnertag || true
    docker rmi $shelltag || true
    docker rmi $postgrestag || true
}

buildImageIfMissing() {
    BANG=$(mktemp -d)
    trap 'rm -rf $BANG' EXIT
    if [[ "$(docker images -q $runnertag 2> /dev/null)" == "" ]]; then
        if [[ "$(docker images -q $shelltag 2> /dev/null)" == "" ]]; then
            BANGSHELL="$BANG/shell"
            mkdir -p $BANGSHELL
            git clone https://github.com/armijnhemel/binaryanalysis-ng $BANGSHELL
            docker build -t $shelltag $BANGSHELL
        else
            echo "docker image $shelltag already build"
        fi
        BANGRUNNER="$BANG/runner"
        mkdir -p $BANGRUNNER
        cat <<EOF >"$BANGRUNNER/bang.config"
[configuration]
baseunpackdirectory = /out
temporarydirectory = /out
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
postgresql_error_fatal = yes
postgresql_host = postgres
postgresql_port = 5432

[elasticsearch]
elastic_enabled = no
elastic_user = bang
elastic_password = bangbang
elastic_index = bang
elastic_connectionerrorfatal = yes
elastic_host = elastic
elastic_port = 9200
EOF
        cat <<EOF >"$BANGRUNNER/entrypoint.sh"
#!/usr/bin/env bash
set -ex
# python3 /binaryanalysis-ng-maintenance/database/nsrlimporter.py \
#         -c /bang.config \
#         -d /out
sleep 2
python3 bang-scanner \
        -d /out \
        -t /out \
        -c /bang.config \
        -d /in
EOF
        git clone https://github.com/armijnhemel/binaryanalysis-ng-maintenance $BANGRUNNER/binaryanalysis-ng-maintenance
        cat <<EOF >"$BANGRUNNER/Dockerfile"
FROM $shelltag
RUN dnf update -y && \
    dnf install -y binutils
COPY ./binaryanalysis-ng-maintenance /binaryanalysis-ng-maintenance
COPY bang.config /
COPY entrypoint.sh /
RUN set -x \
 && chmod +x /entrypoint.sh
ENTRYPOINT /entrypoint.sh
EOF
        docker build -t "$runnertag" "$BANGRUNNER"
    else
        echo "docker image $runnertag already build"
    fi
    if [[ "$(docker images -q $postgrestag 2> /dev/null)" == "" ]]; then
        set -ex
        BANGPOSTGRES="$BANG/postgres"
        git clone https://github.com/armijnhemel/binaryanalysis-ng-maintenance $BANGPOSTGRES
        cat <<EOF >$BANGPOSTGRES/Dockerfile
FROM postgres:latest
RUN apt-get update \
 && apt-get install -y python3
COPY database/nsrl-init.sql /docker-entrypoint-initdb.d/
EOF
        docker build -t "$postgrestag" "$BANGPOSTGRES"
    fi
}

getOutFolder() {
    local in="$(readlink -f "$1")"
    local out="${in%_bang}_bang"
    mkdir -p "$out"
    echo "$out"
}

runBang() {
    local in="$(readlink -f "$1")"
    shift
    [[ ! -d "$in" ]] && exit 1
    local out="$(getOutFolder "$in")"

    cat <<EOF >"$out/docker-compose.yml"
version: "3.7"
services:
  runner:
    image: $runnertag
    depends_on:
      - postgres
      - redis
    volumes:
      - $in:/in:ro
      - $out:/out
  postgres:
    image: $postgrestag
    environment:
      POSTGRES_DB: bang
      POSTGRES_USER: bang
      POSTGRES_PASSWORD: bang
    volumes:
      - $out/_postgres:/var/lib/postgresql/data
  redis:
    image: redis:latest
    volumes:
      - $out/_redis:/data
EOF
    (cd "$out";
     docker-compose up \
                    --abort-on-container-exit \
                    --exit-code-from postgres;
     docker-compose down)
}

#################################################################################
# main
#################################################################################
if [[ "$1" == "-fr" ]]; then
    shift
    forceRebuild
fi
buildImageIfMissing

runBang "$@"

