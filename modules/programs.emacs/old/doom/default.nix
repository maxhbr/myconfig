# partially based on:
# https://gist.github.com/dltacube/280c82b3426690558341a3ac3a71428d
{
  inputs,
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig;
in
{
  options.myconfig = with lib; {
    editor.emacs.doom.enable = mkEnableOption "emacs";
  };
  config = lib.mkIf config.myconfig.editor.emacs.doom.enable {
    home-manager.sharedModules = [
      (
        { config, ... }:
        {
          home = {
            sessionPath = [
              "${config.xdg.configHome}/emacs/bin"
              "${config.home.homeDirectory}/.emacs.d/bin"
              "${pkgs.emacs}/bin"
              "${pkgs.git}"
            ];
            sessionVariables = {
              DOOMDIR = "${config.xdg.configHome}/doom-config";
              DOOMLOCALDIR = "${config.xdg.configHome}/doom-local";
              #DOOMLOCALDIR = "${config.home.homeDirectory}/.emacs.d";
            };
          };
          xdg = {
            enable = true;
            configFile = {
              "doom-config" = {
                source = ./doom.d;
                recursive = true;
              };
              "emacs" = {
                source = inputs.doomemacs;
                recursive = true;
                onChange = "${pkgs.writeShellScript "doom-change" ''
                  export DOOMDIR="${config.home.sessionVariables.DOOMDIR}"
                  export DOOMLOCALDIR="${config.home.sessionVariables.DOOMLOCALDIR}"
                  export PATH="$PATH:${pkgs.emacs}/bin"
                  export PATH="$PATH:${pkgs.git}/bin"
                  export PATH="$PATH:${pkgs.imagemagick}/bin"
                  export PATH="$PATH:${pkgs.ripgrep}/bin"
                  export PATH="$PATH:${pkgs.fd}/bin"
                  export PATH="$PATH:${pkgs.sqlite}/bin"
                  if [[ ! -d "$DOOMLOCALDIR" ]]; then
                    ${config.xdg.configHome}/emacs/bin/doom install --force
                  else
                    ${config.xdg.configHome}/emacs/bin/doom --force clean
                    ${config.xdg.configHome}/emacs/bin/doom --force sync -u
                  fi
                ''}";
              };
            };
          };
        }
      )
    ];
  };
}
