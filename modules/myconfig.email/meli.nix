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
  config = lib.mkIf (cfg.email.enable && (builtins.elem "meli" cfg.email.clients)) {
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
          config = lib.mkIf (config.programs.meli.enable && config.home.username == myconfig.user) {
            programs.meli = {
              enable = true;
            };
            myconfig.homeManagerEmailConfig = [
              {
                meli = {
                  enable = true;
                };
              }
            ];
          };
        }
      )
    ];
  };
}
