{ config, lib, pkgs, ... }:
{
  options = {
    myconfig.roles.imagework = {
      enable = lib.mkEnableOption "Imagework role";
    };
  };

  config = lib.mkIf config.myconfig.roles.imagework.enable {
    environment.systemPackages = with pkgs; [
      gimp-with-plugins
      rawtherapee
      geeqie
      #krita

      inkscape

      blender
      librecad
    ];
  };
}
