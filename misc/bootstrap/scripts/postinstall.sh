#!/bin/sh

set -ex

cd /home/mhuber/myconfig

./nixos/writeHostName.sh minimal
mv /etc/nixos/configuration.nix /etc/nixos/configuration.old.nix
sed -i '/mhuber/d' /etc/nixos/configuration.old.nix

git config user.email "packer@myconfig"
git config user.name "packer"

MYCONFIG_ARGS="--fast" nix/default.sh
MYCONFIG_ARGS="--fast" NIXOS_REBUILD_CMD="boot" nixos/default.sh

chown -R mhuber:mhuber /home/mhuber/myconfig

reboot

