{ config, lib, pkgs, ... }:

let
  cfg = config.myconfig;
  user = cfg.user;
in {
  options.myconfig = with lib; {
    headless.enable = mkEnableOption "headless.desktop";
  };
  config = (lib.mkIf cfg.headless.enable {
  });
}
