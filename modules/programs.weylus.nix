{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.myconfig;
in
{
  options.myconfig = with lib; {
    weylus.enable = mkEnableOption "weylus";
  };
  config = (
    lib.mkIf (cfg.weylus.enable) {
      home-manager.sharedModules = [ { home.packages = with pkgs; [ weylus ]; } ];
      networking.firewall.allowedTCPPorts = [
        1701
        9001
      ];
    }
  );
}
