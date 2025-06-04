{ pkgs, config, lib, myconfig, inputs, ... }:
let
  user = myconfig.user;
  persistentDir = "/persistent";
  persistentPrivDir = "${persistentDir}/priv";
  persistentWorkDir = "${persistentDir}/work";
  persistentCacheDir = "${persistentDir}/cache";

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

      # Validate all paths
      validate = paths:
        if paths == [ ] then
          true
        else
          checkPrefixes (lib.head paths) (lib.tail paths)
          && validate (lib.tail paths);
    in if validate sortedPaths then paths else throw "Path validation failed";

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

