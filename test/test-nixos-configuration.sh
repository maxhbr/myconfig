#!/usr/bin/env bash
# see also: https://nixos.mayflower.consulting/blog/2018/09/11/custom-images/

. "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")/../common.sh"

set -ex

if [[ "$1" == "--ci" ]]; then
    cat <<EOF > "$myconfigDir/imports/dummy-hardware-configuration.nix"
{...}: {
  fileSystems."/" = { device = "/dev/sda1"; fsType = "ext4";};
  fileSystems."/boot" ={ device = "/dev/sda1"; fsType = "vfat";};
  boot.loader.grub.devices = [ "/dev/sda1" ];
}
EOF
fi

time \
    nix-build '<nixpkgs/nixos>' \
              $NIX_PATH_ARGS \
              -A system \
              --show-trace --keep-failed \
              --arg configuration "$myconfigDir/default.nix"
