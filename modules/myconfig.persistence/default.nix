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
    ./impermanence.nix
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
    home-manager.sharedModules = [
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
      })
    ];
  };
}
