{ pkgs, config, lib, myconfig, inputs, ... }:
let
  persistentDir = "/persistent";
  persistentPrivDir = "${persistentDir}/priv";
  persistentWorkDir = "${persistentDir}/work";
  persistentCacheDir = "${persistentDir}/cache";
in {
  imports = [ inputs.impermanence.nixosModule ];
  options = {
    myconfig.persistence.impermanence.enable =
      lib.mkEnableOption "impermanence";
  };
  config = lib.mkIf config.myconfig.persistence.impermanence.enable {
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
        users.mhuber = {
          directories = [
            # TODO: module in home-manager can't use `mode`?
            # TODO: parent directories are owned by root
            {
              directory = ".gnupg";
              mode = "0700";
              user = "mhuber";
              group = "mhuber";
            }
            {
              directory = ".ssh";
              mode = "0700";
              user = "mhuber";
              group = "mhuber";
            }
            {
              directory = ".local/share/keyrings";
              mode = "0700";
              user = "mhuber";
              group = "mhuber";
            }
            {
              directory = ".password-store";
              mode = "0700";
              user = "mhuber";
              group = "mhuber";
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
    home-manager.sharedModules = [
      inputs.impermanence.homeManagerModules.impermanence
      ({ config, ... }: {
        config = {
          home.persistence."${persistentPrivDir}/home/${config.home.username}" =
            {
              directories = config.myconfig.persistence.directories ++ [
                "myconfig"
                "Downloads"
                "Documents"
                "MINE"
                "bin"
                "_screenshots"
                ".local/share/fish"
              ];
              files = config.myconfig.persistence.files;
              allowOther = true;
            };
          home.persistence."${persistentWorkDir}/home/${config.home.username}" =
            {
              directories = config.myconfig.persistence.work-directories;
              files = config.myconfig.persistence.work-files;
              allowOther = true;
            };
          home.persistence."${persistentCacheDir}/home/${config.home.username}" =
            {
              directories = config.myconfig.persistence.cache-directories;
              files = config.myconfig.persistence.cache-files;
              allowOther = true;
            };
        };
      })
    ];
    system.activationScripts = {
      script.text = ''
        install -d -m 700 "/persistent/priv/home/mhuber" -o ${
          toString config.users.extraUsers.mhuber.uid
        } -g ${toString config.users.extraGroups.mhuber.gid}
        install -d -m 700 "/persistent/work/home/mhuber" -o ${
          toString config.users.extraUsers.mhuber.uid
        } -g ${toString config.users.extraGroups.mhuber.gid}
        install -d -m 700 "/persistent/cache/home/mhuber" -o ${
          toString config.users.extraUsers.mhuber.uid
        } -g ${toString config.users.extraGroups.mhuber.gid}
      '';
    };
  };
}

