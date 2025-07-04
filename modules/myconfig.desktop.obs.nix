# see:
# - https://github.com/NixOS/nixpkgs/pull/85690
# - https://github.com/colemickens/nixcfg
{ pkgs, config, lib, ... }:
let cfg = config.myconfig;
in {
  options.myconfig = with lib; { desktop.obs.enable = mkEnableOption "obs"; };

  config = (lib.mkIf (cfg.desktop.enable && cfg.desktop.obs.enable) {
    # myconfig.v4l2.enable = true;
    home-manager.sharedModules = [{
      programs.obs-studio = {
        enable = true;
        plugins = with pkgs.obs-studio-plugins; [ wlrobs obs-ndi ];
      };
      home.packages = with pkgs; [ ndi ];
    }];
  });
}
