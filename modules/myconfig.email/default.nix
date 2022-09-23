{ config, lib, pkgs, ... }:

let cfg = config.myconfig;
in {
  options.myconfig = with lib; {
    email.enable = mkEnableOption "myconfig.email";
  };
  config = lib.mkIf cfg.email.enable {
    home-manager.sharedModules = [ ./home-manager.email ];
  };
}
