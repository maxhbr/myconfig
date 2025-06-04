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
  nixosConfig = config;
in {
  imports = [ ./impermanence.nix ];
  options = {
    myconfig.persistence.directories = optionDirectories;
    myconfig.persistence.files = optionFiles;
    myconfig.persistence.work-directories = optionDirectories;
    myconfig.persistence.work-files = optionFiles;
    myconfig.persistence.cache-directories = optionDirectories;
    myconfig.persistence.cache-files = optionFiles;
  };
  config = {
    home-manager.sharedModules = [
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
          # take from nixos config
          myconfig.persistence.directories =
            nixosConfig.myconfig.persistence.directories;
          myconfig.persistence.files = nixosConfig.myconfig.persistence.files;
          myconfig.persistence.work-directories =
            nixosConfig.myconfig.persistence.work-directories;
          myconfig.persistence.work-files =
            nixosConfig.myconfig.persistence.work-files;
          myconfig.persistence.cache-directories =
            nixosConfig.myconfig.persistence.cache-directories;
          myconfig.persistence.cache-files =
            nixosConfig.myconfig.persistence.cache-files;
        };
      })
    ];
  };
}
