{
  config,
  pkgs,
  lib,
  ...
}:
{
  config = {
    home-manager.sharedModules = [
      (
        { config, ... }: {
          programs.mr = {
            enable = lib.mkDefault true;
            settings = {
              "mr/myconfig/" = {
                checkout = "git clone https://github.com/maxhbr/myconfig";
                update = "git pull --rebase";
              };
            };
          };
        }
      )
    ];
  };
}
