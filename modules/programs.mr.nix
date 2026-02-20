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
              "myconfig/myconfig/" = {
                checkout = "git clone https://github.com/maxhbr/myconfig";
                update = "git pull --rebase";
              };
              "myconfig/myphoto/" = {
                checkout = "git clone https://github.com/maxhbr/myphoto";
                update = "git pull --rebase";
              };
            };
          };
        }
      )
    ];
  };
}
