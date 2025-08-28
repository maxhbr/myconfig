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
              # hooks = {
              # #   preNew = "${config.programs.mbsync.package}/bin/mbsync --all";
              #   postNew = if config.programs.afew.enable
              #             then "afew --tag --new"
              #             else "";
              # };
            };
          };
        }
      )
    ];
  };
}
