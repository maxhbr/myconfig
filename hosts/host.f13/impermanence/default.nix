{ pkgs, config, lib, myconfig, inputs, ... }:
let
  optionDirectories = with lib;
    mkOption {
      type = types.listOf (types.str);
      description = "Directories to persist";
      default = [ ];
    };
  optionFiles = with lib;
    mkOption {
      type = types.listOf (types.str);
      description = "Files to persist";
      default = [ ];
    };
in {
  imports = [
    inputs.impermanence.nixosModule
    ./clean_home.nix
    ./myconfig.persistence.nix
  ];
  options = {
    myconfig.persistence.directories = optionDirectories;
    myconfig.persistence.files = optionFiles;
    myconfig.persistence.work-directories = optionDirectories;
    myconfig.persistence.work-files = optionFiles;
    myconfig.persistence.cache-directories = optionDirectories;
    myconfig.persistence.cache-files = optionFiles;
  };
  config = {
    environment.persistence."/persistent/priv" = {
      enable = true;
      hideMounts = true;
      directories = [
        "/var/lib/bluetooth"
        "/var/lib/nixos"
        "/etc/NetworkManager/system-connections"
        "/etc/ssh/authorized_keys"
      ] ++ (if config.services.ollama.enable then [{
        file = "/var/lib/private/ollama";
        parentDirectory = { mode = "u=rwx,g=,o="; };
      }] else
        [ ]);
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
    environment.persistence."/persistent/cache" = {
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
    programs.fuse.userAllowOther = true;
    home-manager.sharedModules = [
      inputs.impermanence.homeManagerModules.impermanence
      {
        # take from nixos config
        myconfig.persistence.directories =
          config.myconfig.persistence.directories;
        myconfig.persistence.files = config.myconfig.persistence.files;
        myconfig.persistence.work-directories =
          config.myconfig.persistence.work-directories;
        myconfig.persistence.work-files =
          config.myconfig.persistence.work-files;
        myconfig.persistence.cache-directories =
          config.myconfig.persistence.cache-directories;
        myconfig.persistence.cache-files =
          config.myconfig.persistence.cache-files;
      }
      ({ config, ... }: {
        options = {
          myconfig.persistence.directories = optionDirectories;
          myconfig.persistence.files = optionFiles;
          myconfig.persistence.work-directories = optionDirectories;
          myconfig.persistence.work-files = optionFiles;
          myconfig.persistence.cache-directories = optionDirectories;
          myconfig.persistence.cache-files = optionFiles;
        };
        config = {
          home.persistence."/persistent/priv/home/${config.home.username}" = {
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
          home.persistence."/persistent/work/home/${config.home.username}" = {
            directories = config.myconfig.persistence.work-directories;
            files = config.myconfig.persistence.work-files;
            allowOther = true;
          };
          home.persistence."/persistent/cache/home/${config.home.username}" = {
            directories = config.myconfig.persistence.cache-directories;
            files = config.myconfig.persistence.cache-files;
            allowOther = true;
          };
        };
      })
    ];
  };
}
