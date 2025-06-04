{ config, myconfig, lib, pkgs, ... }:  let
  user = myconfig.user;
  btrfs_device = config.fileSystems."/home".device;
in {
  # see https://github.com/nix-community/impermanence?tab=readme-ov-file
  # + patches
  config = {
    myconfig.persistence.impermanence.enable = true;
    boot.initrd.postResumeCommands = lib.mkAfter ''
      clean_home() {
        if [[ -e /btrfs_tmp/@home ]]; then
            mkdir -p /btrfs_tmp/old_homes
            timestamp=$(date --date="@$(stat -c %Y /btrfs_tmp/@home)" "+%Y-%m-%d_%H:%M:%S")
            if [[ ! -e /btrfs_tmp/old_homes/$timestamp ]]; then
              mv /btrfs_tmp/@home "/btrfs_tmp/old_homes/$timestamp"
            else
              btrfs subvolume delete /btrfs_tmp/@home
            fi
        fi
      }

      delete_subvolume_recursively() {
          IFS=$'\n'

          # If we accidentally end up with a file or directory under old_homes,
          # the code will enumerate all subvolumes under the main volume.
          # We don't want to remove everything under true main volume. Only
          # proceed if this path is a btrfs subvolume (inode=256).
          if [ $(stat -c %i "$1") -ne 256 ]; then return; fi

          for i in $(btrfs subvolume list -o "$1" | cut -f 9- -d ' '); do
              delete_subvolume_recursively "/btrfs_tmp/$i"
          done
          btrfs subvolume delete "$1"
      }

      delete_old_snapshots() {
        latest_snapshot=$(find /btrfs_tmp/old_homes/ -mindepth 1 -maxdepth 1 -type d | sort -r | head -n 1)
        # Only delete old snapshots if there's at least one that will remain after deletion
        if [ -n "$latest_snapshot" ]; then
            for i in $(find /btrfs_tmp/old_homes/ -mindepth 1 -maxdepth 1 -mtime +30 | grep -v -e "$latest_snapshot"); do
                delete_subvolume_recursively "$i"
            done
        fi
      }

      init_home() {
        btrfs subvolume create /btrfs_tmp/@home
        install -d -m 700 "/btrfs_tmp/@home/${user}" -o ${
          toString config.users.extraUsers.${user}.uid
        } -g ${toString config.users.extraGroups.${user}.gid}
        for folder in .local .local/share .config; do
          install -d -m 755 "/btrfs_tmp/@home/${user}/$folder" -o ${
            toString config.users.extraUsers.${user}.uid
          } -g ${toString config.users.extraGroups.${user}.gid}
        done
      }

      set -x

      mkdir /btrfs_tmp
      mount ${btrfs_device} /btrfs_tmp
      exec 1>>/btrfs_tmp/boot.initrd.postResumeCommands.log 2>&1

      clean_home
      delete_old_snapshots
      init_home

      exec 1>&- 2>&-

      umount /btrfs_tmp

      set +x
    '';
  };
}
