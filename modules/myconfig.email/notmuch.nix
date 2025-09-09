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
  config = lib.mkIf (cfg.email.enable && (cfg.email.indexer == "notmuch")) {
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
            programs.notmuch = {
              enable = true;
            };
            programs.afew.enable = false;
            myconfig.homeManagerEmailConfig = [
              {
                notmuch = {
                  enable = true;
                  neomutt.enable = true;
                };
              }
            ];
          };
        }
      )
    ];
  };
}
