{ pkgs, config, ... }:
let
  chrootPopOS = with pkgs;
    writeShellScriptBin "chrootPopOS" ''
      set -euo pipefail
      set -x

      MNT=/mnt/popos
      DEV=/dev/disk/by-uuid/69049280-aae9-45cf-824a-fecb268f3e08

      if [[ ! -d "$MNT/boot" ]]; then
        sudo mkdir -p $MNT
        sudo mount $DEV $MNT
        cd $MNT

        sudo mount -t proc /proc proc/
        sudo mount --rbind /sys sys/
        sudo mount --rbind /dev dev/
        sudo mount --rbind /dev/pts dev/pts/
        sudo mount --rbind /run run/
        sudo mount --rbind /sys/firmware/efi/efivars sys/firmware/efi/efivars/
      fi

      ${xorg.xhost}/bin/xhost +local:

      sudo ${coreutils}/bin/chroot \
           --userspec=mhuber:mhuber $MNT \
           /bin/env -i \
               HOME=/home/mhuber TERM=$TERM PS1='\u:\w\$ ' \
               DISPLAY=:0 \
               PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin" \
               /bin/bash --login
    '';
in {
  config = {
    home-manager.sharedModules =
      [{ home.packages = with pkgs; [ chrootPopOS ]; }];
  };
}
