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
    ({config, lib, ...}: {
      config = lib.mkIf config.programs.steam.enable {
        home-manager.sharedModules = [
          ({config, ...}: {
            myconfig.persistence.cache-directories = [
              ".local/share/Steam"
            ];
          })
        ];
      };
    })
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
      users.mhuber = {
        directories = [
          # TODO: module in home-manager can't use `mode`?
          { directory = ".gnupg"; mode = "0700"; user = "mhuber"; group = "mhuber"; }
          { directory = ".ssh"; mode = "0700"; user = "mhuber"; group = "mhuber"; }
          { directory = ".local/share/keyrings"; mode = "0700"; user = "mhuber"; group = "mhuber"; }
          { directory = ".password-store"; mode = "0700"; user = "mhuber"; group = "mhuber"; }
        ] ++ (if config.services.syncthing.enable then 
          ((lib.mapAttrsToList (name: folder: let 
              path = if lib.hasPrefix "/home/mhuber/" name then lib.removePrefix "/home/mhuber/" name else folder.path;
            in { directory = path; mode = "0700"; user = "mhuber"; group = "mhuber"; }) config.services.syncthing.folders) ++ [ { directory = "syncthing"; mode = "0700"; user = "mhuber"; group = "mhuber"; } ])
         else []);
      };
    };
    environment.persistence."/persistent/cache" = {
      enable = true;
      hideMounts = true;
      directories = [
        { directory = "/var/lib/private"; mode = "0700"; }
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
    home-manager.sharedModules = [
        inputs.impermanence.homeManagerModules.impermanence
        ({lib, options, ...}: let
          optionDirectories = with lib; mkOption {
            type = types.listOf (types.str);
            description = "Directories to persist";
            default = [];
          };
          in {
          options = {
            myconfig.persistence.directories = optionDirectories;
            myconfig.persistence.work-directories = optionDirectories;
            myconfig.persistence.cache-directories = optionDirectories;
          };
        })
        ({config, ...}: {
        home.persistence."/persistent/priv/home/${config.home.username}" = {
          directories = config.myconfig.persistence.directories ++ [
            "myconfig"
            "Downloads"
            "Documents"
            "MINE"
            "bin"
            "_screenshots"
            "Maildir/alfa"
            "Maildir/gmail"
            "Maildir/mail"
            ".config/Signal"
            ".mozilla"
            ".config/chromium"
            # ".local/share/fish"
            ".config/Joplin"
            ".config/joplin-desktop"
          ];
          files = [
            ".local/share/fish/fish_history"
          ];
          allowOther = true;
        };
        home.persistence."/persistent/work/home/${config.home.username}" = {
          directories = config.myconfig.persistence.work-directories ++ [
            "TNG"
            "Maildir/tng"
            ".config/teams-for-linux"
            ".zoom/data"
            ".config/Slack"
          ];
          files = [
            ".config/zoom.conf"
            ".config/zoomus.conf"
          ];
        };
        home.persistence."/persistent/cache/home/${config.home.username}" = {
          directories = config.myconfig.persistence.cache-directories ++ [
            ".config/Cursor"
            ".cursor"
          ];
        };
      })
    ];
  };
}