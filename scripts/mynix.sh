#!/usr/bin/env sh
# based on / copied from:
# - http://yann.hodique.info/blog/nix-in-custom-location/
# - https://github.com/sigma/docker-mynix/
set -e

createTGZ() {
    set -x

    # create target nix location
    mkdir -p ${NIX_NEW_HOME}/overlays
    mkdir -p ${NIX_NEW_HOME}/store
    mkdir -p ${NIX_NEW_HOME}/var

    # create overlay for custom paths
    cat <<EOF > ${NIX_NEW_HOME}/overlays/01-nix-path.nix
self: super:
{
    nix = super.nix.override {
        storeDir = "${NIX_NEW_HOME}/store";
        stateDir = "${NIX_NEW_HOME}/var";
    };
}
EOF

    # make sure we have the latest nix. In particular we want overlays (in nix 1.11.8+)
    nix-channel --update && nix-env -u -j $(nproc)

    mkdir -p ${HOME}/.config/nixpkgs/
    ln -s ${NIX_NEW_HOME}/overlays ${HOME}/.config/nixpkgs/overlays

    # create a nix version that targets the custom location
    nix-env -i nix nss-cacert -j $(nproc)

    # create a nix version that also *lives* in the custom location
    nix-env -i nix nss-cacert -j $(nproc)

    # find the user-environment that just got created
    USER_ENV=$(find ${NIX_NEW_HOME}/store/ -name "*-user-environment")

    ln -s ${USER_ENV} ${NIX_NEW_HOME}/var/nix/profiles/default-1-link
    ln -s ${NIX_NEW_HOME}/var/nix/profiles/default-1-link ${NIX_NEW_HOME}/var/nix/profiles/default

    # package the whole thing
    tar -czf /target/mynix.tgz --owner=${NIX_NEW_UID:-0} --group=${NIX_NEW_GID:-0} ${NIX_NEW_HOME}
}

createContainer() {
    tmp=$(mktemp -d)
    cp $0 $tmp
    cat<<EOF > $tmp/Dockerfile
FROM nixos/nix
RUN apk add --update coreutils

VOLUME /target
ADD $(basename $0) /entrypoint.sh

CMD [ "/entrypoint.sh", "createTGZ" ]
EOF
    docker build -t mhuber/mynix --rm=true --force-rm=true $tmp
}

runContainer() {
    new_home=$(pwd)
    docker run -ti -v "$(pwd):/target" \
           -e NIX_NEW_HOME=$new_home/nix \
           -e NIX_NEW_UID=$(id -u) \
           -e NIX_NEW_GID=$(id -g) mhuber/mynix
}

bootstrap() {
    return 1

    new_home=$(pwd)
    cd /
    tar -zxf "$new_home/mynix.tgz"
    ln -s ${new_home}/nix/var/nix/profiles/default ~/.nix-profile
    mkdir -p ~/.config/nixpkgs
    ln -s ${new_home}/nix/overlays ~/.config/nixpkgs/overlays
}

if [ "$1" == "createTGZ" ]; then
    createTGZ
else
    createContainer
    runContainer
    if [ "$1" == "boostrap" ]; then
        bootstrap
    fi
fi
