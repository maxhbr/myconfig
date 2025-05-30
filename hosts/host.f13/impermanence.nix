{ pkgs, config, lib, myconfig, inputs, ... }: {
  imports = [
    inputs.impermanence.nixosModule
    {
      # see https://github.com/nix-community/impermanence?tab=readme-ov-file
      # + patches
      config = let 
          btrfs_device = config.fileSystems."/home".device;
        in {
          boot.initrd.postResumeCommands = lib.mkAfter ''
            set -x

            mkdir /btrfs_tmp
            mount ${btrfs_device} /btrfs_tmp
            exec 1>>/btrfs_tmp/boot.initrd.postResumeCommands.log 2>&1
            if [[ -e /btrfs_tmp/@home ]]; then
                mkdir -p /btrfs_tmp/old_homes
                timestamp=$(date --date="@$(stat -c %Y /btrfs_tmp/@home)" "+%Y-%m-%d_%H:%M:%S")
                if [[ ! -e /btrfs_tmp/old_homes/$timestamp ]]; then
                  mv /btrfs_tmp/@home "/btrfs_tmp/old_homes/$timestamp"
                else
                  btrfs subvolume delete /btrfs_tmp/@home
                fi
            fi

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

            latest_snapshot=$(find /btrfs_tmp/old_homes/ -mindepth 1 -maxdepth 1 -type d | sort -r | head -n 1)
            # Only delete old snapshots if there's at least one that will remain after deletion
            if [ -n "$latest_snapshot" ]; then
                for i in $(find /btrfs_tmp/old_homes/ -mindepth 1 -maxdepth 1 -mtime +30 | grep -v -e "$latest_snapshot"); do
                    delete_subvolume_recursively "$i"
                done
            fi

            btrfs subvolume create /btrfs_tmp/@home

            exec 1>&- 2>&-

            umount /btrfs_tmp

            # Create a new home in subvolume
            # TODO: why is this needed?
            mount -o "subvol=@home" ${btrfs_device} /btrfs_tmp/
            mkdir -m 700 /btrfs_tmp/mhuber
            chown 1000:1000 /btrfs_tmp/mhuber
            umount /btrfs_tmp

            set +x
          '';
      };
    }
  ];
  config = {
    environment.persistence."/persistent/priv" = {
      enable = true;
      hideMounts = true;
      directories = [
        "/var/lib/bluetooth"
        "/var/lib/nixos"
        "/etc/NetworkManager/system-connections"
        "/etc/ssh/authorized_keys"
      ] ++ (if config.services.ollama.enable then [
        { file = "/var/lib/private/ollama"; parentDirectory = { mode = "u=rwx,g=,o="; }; }
      ] else []);
      files = [
        # "/etc/machine-id"
        { file = "/var/keys/secret_file"; parentDirectory = { mode = "u=rwx,g=,o="; }; }
        "/etc/ssh/ssh_host_ed25519_key"
        "/etc/ssh/ssh_host_ed25519_key.pub"
        "/etc/ssh/ssh_host_rsa_key"
        "/etc/ssh/ssh_host_rsa_key.pub"
      ];
    };
    environment.persistence."/persistent/cache" = {
      enable = true;
      hideMounts = true;
      directories = [
        # "/var/lib/ollama"
        "/var/lib/systemd/coredump"
      ];
    };
    system.activationScripts = {
      script.text = ''
        install -d -m 755 "/persistent/priv/home/mhuber" -o ${toString config.users.extraUsers.mhuber.uid} -g ${toString config.users.extraGroups.mhuber.gid}
        install -d -m 755 "/persistent/work/home/mhuber" -o ${toString config.users.extraUsers.mhuber.uid} -g ${toString config.users.extraGroups.mhuber.gid}
        install -d -m 755 "/persistent/cache/home/mhuber" -o ${toString config.users.extraUsers.mhuber.uid} -g ${toString config.users.extraGroups.mhuber.gid}
      '';
    };
    programs.fuse.userAllowOther = true;
    home-manager.users.mhuber = {...}: {
      imports = [
        inputs.impermanence.homeManagerModules.impermanence
      ];
      home.persistence."/persistent/priv/home/mhuber" = {
        directories = [
          "myconfig"
          "Downloads"
          "Documents"
          "MINE"
          "_screenshots"
          ".gnupg"
        #   ".ssh"
        #   ".nixops"
        #   ".local/share/keyrings"
        #   ".local/share/direnv"
        #   {
        #     directory = ".local/share/Steam";
        #     method = "symlink";
        #   }
          "Maildir/alfa"
          "Maildir/gmail"
          "Maildir/mail"
        ];
        # files = [
        #   ".screenrc"
        # ];
        allowOther = true;
      };
      home.persistence."/persistent/work/home/mhuber" = {
        directories = [
          "TNG"
          "Maildir/tng"
        ];
      };
    };
  };
}