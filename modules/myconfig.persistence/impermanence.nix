{ pkgs, config, lib, myconfig, inputs, ... }:
let
  user = myconfig.user;
  persistentDir = "/persistent";
  persistentPrivDir = "${persistentDir}/priv";
  persistentWorkDir = "${persistentDir}/work";
  persistentCacheDir = "${persistentDir}/cache";
  volumeLog = "@log";
  volumeNix = "@nix";
  volumeHome = "@home";
  volumePriv = "@persistent_priv";
  volumeWork = "@persistent_work";
  volumeCache = "@persistent_cache";

  validateDevice = device:
    if lib.isString device && lib.hasPrefix "/dev/" device then
      device
    else
      throw
      "Invalid device: '${device}' (must be a string and start with '/dev/')";

  validatePaths = paths:
    let
      sortedPaths = lib.sort (a: b: lib.hasPrefix a b) paths;

      # Check if path a is a prefix of path b
      isPrefix = a: b: a != b && lib.hasPrefix a b;

      # Check each path against all following paths in the sorted list
      checkPrefixes = path: rest:
        if rest == [ ] then
          true
        else if isPrefix path (lib.head rest) then
          throw "Invalid paths: '${path}' is a prefix of '${lib.head rest}'"
        else
          checkPrefixes path (lib.tail rest);

      # check that path does not start with / or ~
      isRelativePath = path:
        if lib.hasPrefix "/" path || lib.hasPrefix "~" path then
          throw "Invalid path: '${path}' must be a relative path"
        else
          true;

      # Validate all paths
      validate = paths:
        if paths == [ ] then
          true
        else
          checkPrefixes (lib.head paths) (lib.tail paths)
          && isRelativePath (lib.head paths) && validate (lib.tail paths);
    in if validate sortedPaths then paths else throw "Path validation failed";

in {
  imports = [ 
    inputs.impermanence.nixosModule
    {
      config = lib.mkIf (config.myconfig.persistence.impermanence.btrbk_device != null && config.myconfig.persistence.impermanence.btrbk_luks_device != null) (let
          mountPoint = "/btr_backup";
          mkTarget = name: "${mountPoint}/${config.networking.hostName}-${name}";
          mkSettings = name: subvolume: {
              snapshot_preserve_min = "24h";
              snapshot_preserve     = "7d 4w 6m 1y";   
              volume."/btr_pool" = {
                inherit subvolume;
                snapshot_dir = ".snapshots";
                target = mkTarget name;
              };
            };
          mkScript = name: pkgs.writeShellScriptBin "btrbk-usbhdd-${name}" ''
            set -euo pipefail

            conf="/etc/btrbk/usbhdd-${name}.conf"
            if [ ! -f "$conf" ]; then
              echo "Config file $conf does not exist"
              exit 1
            fi
            
            if [ ! -b "${validateDevice config.myconfig.persistence.impermanence.btrbk_device}" ]; then
              echo "Device ${validateDevice config.myconfig.persistence.impermanence.btrbk_device} does not exist"
              if [ ! -b "${validateDevice config.myconfig.persistence.impermanence.btrbk_luks_device}" ]; then
                echo "Device ${validateDevice config.myconfig.persistence.impermanence.btrbk_luks_device} does not exist"
                exit 1
              fi
              echo "Decrypting ${validateDevice config.myconfig.persistence.impermanence.btrbk_luks_device}"
              sudo cryptsetup luksOpen "${validateDevice config.myconfig.persistence.impermanence.btrbk_luks_device}" "btr_backup_luks"

              if [ ! -b "${validateDevice config.myconfig.persistence.impermanence.btrbk_device}" ]; then
                echo "Device ${validateDevice config.myconfig.persistence.impermanence.btrbk_device} still does not exist"
                exit 1
              fi
            fi

            if ! mountpoint -q "${mountPoint}"; then
              echo "Mounting ${mountPoint}"
              sudo mount "${validateDevice config.myconfig.persistence.impermanence.btrbk_device}" "${mountPoint}"
            fi

            target=${mkTarget name}
            if [ ! -d "$target" ]; then
              sudo mkdir -p "$target"
            fi

            set -x
            sudo -H -u btrbk bash -c "btrbk -c $conf --progress --verbose run"
          '';
        in {
        fileSystems."${mountPoint}" = {
          device  = validateDevice config.myconfig.persistence.impermanence.btrbk_device;
          fsType  = "btrfs";
          options = [
            "subvolid=5"          # show all subvolumes
            "compress=zstd"
            "nofail"              # don’t block boot if the disk is absent
            "x-systemd.automount" # mount lazily the first time it’s accessed
          ];
        };
        services.btrbk = {
          instances = {
            "usbhdd-priv" = {
              onCalendar = null;
              settings = mkSettings "priv" volumePriv;
            };
            "usbhdd-work" = {
              onCalendar = null;
              settings = mkSettings "work" volumeWork;
            };
          };
        };
        systemd.tmpfiles.rules = [
          "d /btr_pool/.snapshots 0755 root root"
        ];

        environment.systemPackages = [ pkgs.lz4 (mkScript "priv") (mkScript "work") ];
      });
    }
  ];
  options = with lib; {
    myconfig.persistence.impermanence.enable =
      lib.mkEnableOption "impermanence";
    myconfig.persistence.impermanence.btrfs_device = mkOption {
      default = null;
      example = "/dev/sda";
      type = types.nullOr types.str;
      description = "Location of the device.";
    };
    myconfig.persistence.impermanence.btrbk_device = mkOption {
      default = null;
      example = "/dev/disk/by-uuid/8e3c7395-c663-4080-9463-3b8a18bd7ad3";
      type = types.nullOr types.str;
      description = "Location of the btrbk device.";
    };
    myconfig.persistence.impermanence.btrbk_luks_device = mkOption {
      default = null;
      example = "/dev/disk/by-uuid/8e3c7395-c663-4080-9463-3b8a18bd7ad3";
      type = types.nullOr types.str;
      description = "Location of the btrbk LUKS device, which is used to encrypt the btrbk device.";
    };
    # myconfig.persistence.impermanence.btrbk_targets = mkOption {
    #   default = [ ];
    #   type = types.listOf types.str;
    #   description = "List of btrbk targets to backup.";
    # };
    myconfig.persistence.impermanence.enable_smartd =
      mkEnableOption "smartd for the btrfs impermanence device";
  };

  config = lib.mkIf config.myconfig.persistence.impermanence.enable {
    # see https://github.com/nix-community/impermanence?tab=readme-ov-file
    # + patches
    boot.initrd.postResumeCommands = lib.mkAfter ''
      clean_home() {
        if [[ -e /btrfs_tmp/${volumeHome} ]]; then
            mkdir -p /btrfs_tmp/old_homes
            timestamp=$(date --date="@$(stat -c %Y /btrfs_tmp/${volumeHome})" "+%Y-%m-%d_%H:%M:%S")
            if [[ ! -e /btrfs_tmp/old_homes/$timestamp ]]; then
              mv /btrfs_tmp/${volumeHome} "/btrfs_tmp/old_homes/$timestamp"
            else
              btrfs subvolume delete /btrfs_tmp/${volumeHome}
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

      create_subvolume_if_not_exists() {
        if ! btrfs subvolume list /btrfs_tmp | grep -q "$1"; then
          btrfs subvolume create /btrfs_tmp/$1
        fi
      }

      init_home() {
        create_subvolume_if_not_exists "${volumeHome}"
        install -d -m 700 "/btrfs_tmp/${volumeHome}/${user}" -o ${
          toString config.users.extraUsers.${user}.uid
        } -g ${toString config.users.extraGroups.${user}.gid}
        for folder in .local .local/share .config; do
          install -d -m 755 "/btrfs_tmp/${volumeHome}/${user}/$folder" -o ${
            toString config.users.extraUsers.${user}.uid
          } -g ${toString config.users.extraGroups.${user}.gid}
        done
      }

      set -x

      mkdir -p /btrfs_tmp
      mount ${config.myconfig.persistence.impermanence.btrfs_device} /btrfs_tmp
      exec 1>>/btrfs_tmp/boot.initrd.postResumeCommands.log 2>&1

      clean_home
      delete_old_snapshots
      init_home
      create_subvolume_if_not_exists "${volumeLog}"
      create_subvolume_if_not_exists "${volumeNix}"
      create_subvolume_if_not_exists "${volumePriv}"
      create_subvolume_if_not_exists "${volumeWork}"
      create_subvolume_if_not_exists "${volumeCache}"

      exec 1>&- 2>&-

      umount /btrfs_tmp

      set +x
    '';

    fileSystems."/" = {
      device = "none";
      fsType = "tmpfs";
      options = [ "defaults" "size=20%" "mode=755" ];
    };

    fileSystems."/btr_pool" = {
      device =
        validateDevice config.myconfig.persistence.impermanence.btrfs_device;
      fsType = "btrfs";
      options = [ "subvolid=5" ];
    };

    fileSystems."/var/log" = {
      device =
        validateDevice config.myconfig.persistence.impermanence.btrfs_device;
      fsType = "btrfs";
      options = [ "subvol=${volumeLog}" ];
    };

    fileSystems."/nix" = {
      device =
        validateDevice config.myconfig.persistence.impermanence.btrfs_device;
      fsType = "btrfs";
      options = [ "subvol=${volumeNix}" ];
    };

    fileSystems."/home" = {
      device =
        validateDevice config.myconfig.persistence.impermanence.btrfs_device;
      fsType = "btrfs";
      options = [ "subvol=${volumeHome}" ];
    };

    fileSystems."${persistentPrivDir}" = {
      device =
        validateDevice config.myconfig.persistence.impermanence.btrfs_device;
      fsType = "btrfs";
      options = [ "subvol=${volumePriv}" ];
      neededForBoot = true;
    };

    fileSystems."${persistentWorkDir}" = {
      device =
        validateDevice config.myconfig.persistence.impermanence.btrfs_device;
      fsType = "btrfs";
      options = [ "subvol=${volumeWork}" ];
      neededForBoot = true;
    };

    fileSystems."${persistentCacheDir}" = {
      device =
        validateDevice config.myconfig.persistence.impermanence.btrfs_device;
      fsType = "btrfs";
      options = [ "subvol=${volumeCache}" ];
      neededForBoot = true;
    };

    programs.fuse.userAllowOther = true;
    environment.persistence = {
      "${persistentPrivDir}" = {
        enable = true;
        hideMounts = true;
        directories = [
          "/var/lib/bluetooth"
          "/var/lib/nixos"
          "/etc/NetworkManager/system-connections"
          "/etc/ssh/authorized_keys"
        ];
        files = [
          {
            file = "/var/keys/secret_file";
            parentDirectory = { mode = "u=rwx,g=,o="; };
          }
          "/etc/ssh/ssh_host_ed25519_key"
          "/etc/ssh/ssh_host_ed25519_key.pub"
          "/etc/ssh/ssh_host_rsa_key"
          "/etc/ssh/ssh_host_rsa_key.pub"
        ];
        users.${user} = {
          directories = [
            # TODO: module in home-manager can't use `mode`?
            # TODO: parent directories are owned by root
            {
              directory = ".gnupg";
              mode = "0700";
              user = user;
              group = user;
            }
            {
              directory = ".ssh";
              mode = "0700";
              user = user;
              group = user;
            }
            {
              directory = ".local/share/keyrings";
              mode = "0700";
              user = user;
              group = user;
            }
            {
              directory = ".password-store";
              mode = "0700";
              user = user;
              group = user;
            }
          ];
        };
      };
      "/persistent/cache" = {
        enable = true;
        hideMounts = true;
        directories = [
          {
            directory = "/var/lib/private";
            mode = "0700";
          }
          "/var/lib/systemd/coredump"
        ];
      };
    };
    services.smartd = {
      enable =
        lib.mkDefault config.myconfig.persistence.impermanence.enable_smartd;
      devices =
        [{ device = config.myconfig.persistence.impermanence.btrfs_device; }];
    };
    home-manager.sharedModules = [
      inputs.impermanence.homeManagerModules.impermanence
      ({ config, ... }:
        let
          mkRelativeToHome = path:
            if lib.hasPrefix "${config.home.homeDirectory}/" path then
              lib.removePrefix "${config.home.homeDirectory}/" path
            else
              path;
        in {
          config = {
            myconfig.persistence.directories = [
              "myconfig"
              "Downloads"
              "Documents"
              "MINE"
              "bin"
              "_screenshots"
            ];
            myconfig.persistence.cache-directories = [ ".cache/nix-index" ];
            home.persistence."${persistentPrivDir}/home/${config.home.username}" =
              {
                directories = validatePaths (lib.map mkRelativeToHome
                  config.myconfig.persistence.directories);
                files = validatePaths
                  (lib.map mkRelativeToHome config.myconfig.persistence.files);
                allowOther = true;
              };
            home.persistence."${persistentWorkDir}/home/${config.home.username}" =
              {
                directories = validatePaths (lib.map mkRelativeToHome
                  config.myconfig.persistence.work-directories);
                files = validatePaths (lib.map mkRelativeToHome
                  config.myconfig.persistence.work-files);
                allowOther = true;
              };
            home.persistence."${persistentCacheDir}/home/${config.home.username}" =
              {
                directories = validatePaths (lib.map mkRelativeToHome
                  config.myconfig.persistence.cache-directories);
                files = validatePaths (lib.map mkRelativeToHome
                  config.myconfig.persistence.cache-files);
                allowOther = true;
              };
          };
        })
      ({ config, ... }:
        let
          mk_diff_command = dir: name:
            pkgs.writeShellScriptBin "diff_${name}" ''
              set -euo pipefail

              # Variables
              BASE_DIR="${dir}"
              OUTPUT_DIR="$HOME/tmp_diff_${name}"
              TIMESTAMP="$(date +%Y-%m-%d_%H-%M-%S)"
              CURRENT_FILE="$OUTPUT_DIR/$TIMESTAMP"
              CLEANED_FILE="$CURRENT_FILE.cleaned"
              ADDED_FILE="$CURRENT_FILE.added"
              LIST_FILE="$OUTPUT_DIR/list"

              # Ensure output directory exists
              mkdir -p "$OUTPUT_DIR"

              # Find the previous snapshot file (sorted by timestamp)
              PREVIOUS_FILE="$(ls -t "$OUTPUT_DIR" | grep '\.cleaned$' | head -n 2 | tail -n 1 || true)"

              # Generate current file list
              ${pkgs.fd}/bin/fd \
                --one-file-system \
                --base-directory "$BASE_DIR" \
                --type f \
                --hidden \
                --exclude "{$(basename "$OUTPUT_DIR"),.cache}" \
                | while read -r line; do
                  if [[ "$(readlink -f "$line")" == /nix/store/* ]]; then
                      continue
                  fi
                  echo "$line"
                done | sort > "$CLEANED_FILE"
              echo "Current file list saved to: $CLEANED_FILE"
              wc -l "$CLEANED_FILE" | tee -a "$LIST_FILE"

              # If previous exists, compute added files
              if [[ -n "$PREVIOUS_FILE" && -f "$OUTPUT_DIR/$PREVIOUS_FILE" ]]; then
                  comm -13 <(sort "$OUTPUT_DIR/$PREVIOUS_FILE") <(sort "$CLEANED_FILE") > "$ADDED_FILE"
                  echo "Diff saved to: $ADDED_FILE"
                  wc -l "$ADDED_FILE" | tee -a "$LIST_FILE"
              else
                  echo "No previous file to diff against."
              fi
            '';

          diff_home = mk_diff_command "/home/${user}" "home";
          diff_root = mk_diff_command "/" "root";
        in { home.packages = [ diff_home diff_root ]; })
    ];
    system.activationScripts = {
      script.text = ''
        install -d -m 700 "/${persistentPrivDir}/home/${user}" -o ${
          toString config.users.extraUsers.${user}.uid
        } -g ${toString config.users.extraGroups.${user}.gid}
        install -d -m 700 "/${persistentWorkDir}/home/${user}" -o ${
          toString config.users.extraUsers.${user}.uid
        } -g ${toString config.users.extraGroups.${user}.gid}
        install -d -m 700 "/${persistentCacheDir}/home/${user}" -o ${
          toString config.users.extraUsers.${user}.uid
        } -g ${toString config.users.extraGroups.${user}.gid}
      '';
    };
  };
}

