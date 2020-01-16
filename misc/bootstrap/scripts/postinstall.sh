#!/bin/sh

set -ex

cd /home/mhuber/myconfig

hostName="${1:-minimal}"

./lib/writeHostName.sh $hostName
rm /etc/nixos/configuration.nix
rm /etc/nixos/mhuber.nix

git config user.email "${USER}@${hostName}"
git config user.name "${USER}"

export NIXPKGS_ALLOW_UNFREE=1
./rebuild.sh --no-tmux --fast

chown -R mhuber:mhuber /home/mhuber/myconfig

reboot

