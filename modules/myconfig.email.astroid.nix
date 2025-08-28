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
  config = lib.mkIf (cfg.email.enable && (builtins.elem "astroid" cfg.email.clients)) {
    home-manager.sharedModules = [
      (
        { }:
        {
          config = {
            programs.astroid = {
              enable = true;
            };
          };
        }
      )
    ];
  };
}
