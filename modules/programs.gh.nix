{ pkgs, config, ... }:
{
  config = {
    home-manager.sharedModules = [
      (
        { config, lib, ... }:
        {
          config = lib.mkIf config.programs.gh.enable {
            myconfig.persistence.work-directories = [ ".config/gh" ];
          };
        }
      )
    ];
  };
}
