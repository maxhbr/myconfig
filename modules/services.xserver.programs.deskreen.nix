{ config, pkgs, lib, ... }:
let cfg = config.myconfig;
in {
  options.myconfig = with lib; { deskreen.enable = mkEnableOption "deskreen"; };
  config = (lib.mkIf (config.services.xserver.enable && cfg.deskreen.enable) {
    home-manager.sharedModules = [{
      home.packages = with pkgs;
        [
          (writeShellScriptBin "deskreen" ''
            release=2.0.3
            dir="$HOME/.deskreen-cache"
            mkdir -p "$dir"
            ${wget}/bin/wget \
               -O "$dir/Deskreen-$release.AppImage" \
               --no-clobber \
               https://github.com/pavlobu/deskreen/releases/download/v$release/Deskreen-$release.AppImage
            ${appimage-run}/bin/appimage-run "$dir/Deskreen-$release.AppImage"
          '')
        ];
    }];
    networking.firewall.allowedTCPPorts = [ 3131 ];
    networking.firewall.allowedUDPPorts = [ 3131 ];
  });
}
