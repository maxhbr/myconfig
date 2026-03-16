{
  config,
  pkgs,
  lib,
  ...
}:

let
  teams-for-linux-pkg = pkgs.nixos-unstable-small.teams-for-linux;
in
{
  config = {
    home.packages = [
      teams-for-linux-pkg
    ];
    myconfig.persistence.work-directories = [
      ".config/teams-for-linux"
    ];
    myconfig.desktop.wayland.launcherCommands = [
      "teams-for-linux"
    ];
  };
}
