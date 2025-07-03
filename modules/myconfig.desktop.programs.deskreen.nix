{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.myconfig;
  deskreenScript =
    with pkgs;
    writeShellScriptBin "deskreen" ''
      release=2.0.3
      dir="$HOME/.deskreen-cache"
      mkdir -p "$dir"
      ${wget}/bin/wget \
         -O "$dir/Deskreen-$release.AppImage" \
         --no-clobber \
         https://github.com/pavlobu/deskreen/releases/download/v$release/Deskreen-$release.AppImage
      ${appimage-run}/bin/appimage-run "$dir/Deskreen-$release.AppImage"
    '';
  deskreenPkg = pkgs.deskreen;
in
{
  options.myconfig = with lib; {
    desktop.deskreen.enable = mkEnableOption "deskreen";
    desktop.deskreen.usePkgs = mkOption {
      type = types.bool;
      default = true;
      description = "Use the deskreen package from nixpkgs";
    };
  };
  config = (
    lib.mkIf (config.myconfig.desktop.enable && cfg.desktop.deskreen.enable) {
      home-manager.sharedModules = [
        {
          home.packages = if cfg.desktop.deskreen.usePkgs then [ deskreenPkg ] else [ deskreenScript ];
        }
      ];
      networking.firewall.allowedTCPPorts = [ 3131 ];
      networking.firewall.allowedUDPPorts = [ 3131 ];
    }
  );
}
