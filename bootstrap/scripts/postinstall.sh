#!/bin/sh

set -ex

cd /myconfig

./nixos/writeHostName.sh empty
rm /etc/nixos/configuration.nix

git config user.email "packer@myconfig"
git config user.name "packer"

#MYCONFIG_ARGS="--fast"
nix/default.sh
#MYCONFIG_ARGS="--fast"
nixos/default.sh

