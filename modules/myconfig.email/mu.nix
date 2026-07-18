{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig;
in
{
  config = lib.mkIf (cfg.email.enable && (cfg.email.indexer == "mu")) {
    home-manager.sharedModules = [
      (
        {
          config,
          lib,
          pkgs,
          myconfig,
          ...
        }:
        {
          config = lib.mkIf (config.home.username == myconfig.user) {
            programs.mu.enable = true;
            myconfig.persistence.cache-directories = [ ".cache/mu" ];
            myconfig.homeManagerEmailConfig = [
              {
                mu.enable = true;
              }
            ];
          };
        }
      )
    ];
  };
}
