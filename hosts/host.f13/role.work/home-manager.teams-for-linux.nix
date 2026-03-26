{
  config,
  pkgs,
  lib,
  ...
}:

let
  teams-for-linux-pkg = pkgs.teams-for-linux;
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
    programs.zsh.shellAliases = {
      unteams = ''while pkill teams; do echo "kill it with fire!"; done'';
    };
    programs.fish.functions = {
      unteams = ''
        while pkill teams
          echo "kill it with fire!"
        end
        echo "now wo are happy again"
      '';
    };
  };
}
