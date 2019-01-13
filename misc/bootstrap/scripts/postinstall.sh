#!/bin/sh

set -ex

cd /home/mhuber/myconfig

hostName="${1:-minimal}"

./nixos/writeHostName.sh $hostName
mv /etc/nixos/configuration.nix /etc/nixos/configuration.old.nix
# remove the mhuber role to remove redundancy
sed -i '/mhuber/d' /etc/nixos/configuration.old.nix

git config user.email "${USER}@${hostName}"
git config user.name "${USER}"

MYCONFIG_ARGS="--fast" nix/default.sh
MYCONFIG_ARGS="--fast" NIXOS_REBUILD_CMD="boot" nixos/default.sh

chown -R mhuber:mhuber /home/mhuber/myconfig

reboot

