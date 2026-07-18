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
          myconfig,
          ...
        }:
        {
          config = lib.mkIf (config.home.username == myconfig.user) {
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
