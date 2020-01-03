#!/usr/bin/env bash

set -ex

if [ "$(id -u)" -ne "$(stat -c '%u' $0)" ]; then
    echo "you should run this script as the user, which owns $0"
    exit 1
fi
ROOT="$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")"
have() { type "$1" &> /dev/null; }

sudo apt-get update
sudo apt-get upgrade -y
sudo apt-get install -y git vim tmux curl mosh

have nix || {
    curl https://nixos.org/nix/install | sh
}

have flirc || {
    curl apt.flirc.tv/install.sh > /tmp/flirc.install.sh
    chmod +x /tmp/flirc.install.sh
    sudo /tmp/flirc.install.sh -y
}

have wg || {
    sudo add-apt-repository ppa:wireguard/wireguard
    sudo apt-get update
    sudo apt-get install -y wireguard-dkms wireguard-tools
}

if [[ ! -d "$HOME/myconfig" ]]; then
    git clone https://github.com/maxhbr/myconfig ~/myconfig
fi

have docker || {
    # see: https://docs.docker.com/install/linux/docker-ce/ubuntu/

    sudo apt-get install -y \
         apt-transport-https \
         ca-certificates \
         curl \
         gnupg-agent \
         software-properties-common
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -

    sudo add-apt-repository \
         "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
         $(lsb_release -cs) \
         stable"

    sudo apt-get update
    sudo apt-get install -y docker-ce docker-ce-cli containerd.io
}

"$ROOT/bin/openhab.sh"
"$ROOT/bin/deCONZ.sh"

