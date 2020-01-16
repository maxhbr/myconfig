#!/bin/sh

set -ex

cd /home/mhuber/myconfig

hostName="${1:-minimal}"

./lib/writeHostName.sh $hostName
rm /etc/nixos/configuration.nix
mkdir -p /etc/nixos/imports/
mv /etc/nixos/guest.nix /etc/nixos/imports/
mv /etc/nixos/users.nix /etc/nixos/imports/
mv /etc/nixos/vagrant.nix /etc/nixos/imports/
# remove the mhuber role to remove redundancy

git config user.email "${USER}@${hostName}"
git config user.name "${USER}"

export NIXPKGS_ALLOW_UNFREE=1
# MYCONFIG_ARGS="--fast" nix/default.sh
# MYCONFIG_ARGS="--fast" NIXOS_REBUILD_CMD="boot" nixos/default.sh
./rebuild.sh --fast

chown -R mhuber:mhuber /home/mhuber/myconfig

reboot

