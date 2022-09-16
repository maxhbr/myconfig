# see:
# - https://github.com/NixOS/nixpkgs/pull/85690
# - https://github.com/colemickens/nixcfg
{ pkgs, config, lib, ... }: {
  config =
    (lib.mkIf (config.services.xserver.enable && config.myconfig.desktop.full) {
      home-manager.sharedModules = [{
        programs.obs-studio = {
          enable = true;
          plugins = with pkgs.obs-studio-plugins; [ wlrobs ];
        };
      }];
      boot.extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];
      environment.systemPackages = with pkgs; [ v4l-utils ];
    });
}
