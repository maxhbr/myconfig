{ config, lib, inputs, ... }:
let cfg = config.myconfig;
in {
  options.myconfig = with lib; {
    desktop.myphoto.enable = mkEnableOption "myphoto";
  };
  config = (lib.mkIf cfg.desktop.myphoto.enable {
    home-manager.sharedModules = [ inputs.myphoto.homeManagerModules.myphoto ];
  });
}
