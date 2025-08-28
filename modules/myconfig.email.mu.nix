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
          ...
        }:
        {
          config = {
            programs.mu.enable = true;
            myconfig.persistence.cache-directories = [ ".cache/mu" ];
          };
        }
      )
    ];
  };
}
