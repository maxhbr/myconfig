{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.myconfig.desktop.imagework;
  mk_sigal_gallery = pkgs.writeShellApplication {
    name = "mk_sigal_gallery";
    runtimeInputs = with pkgs; [ sigal ];
    text = builtins.readFile ./mk_sigal_gallery.sh;
  };
in
{
  options.myconfig = with lib; {
    desktop.imagework.sigal.enable = mkEnableOption "sigal";
  };
  config = lib.mkIf cfg.sigal.enable {
    home.packages = with pkgs; [
      sigal
      mk_sigal_gallery
    ];
  };
}
