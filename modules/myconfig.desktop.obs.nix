# see:
# - https://github.com/NixOS/nixpkgs/pull/85690
# - https://github.com/colemickens/nixcfg
{ pkgs, config, lib, ... }:
let cfg = config.myconfig;
in {
  options.myconfig = with lib; { desktop.obs.enable = mkEnableOption "obs"; };
  config = (lib.mkIf (cfg.desktop.enable && cfg.desktop.obs.enable) {
    home-manager.sharedModules = [{
      programs.obs-studio = {
        enable = true;
        plugins = with pkgs.obs-studio-plugins; [ wlrobs ];
      };
    }];
    boot = {
      extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];
      kernelModules = [
        "v4l2loopback"
        # Virtual Microphone, built-in
        "snd-aloop"
      ];
      # Set initial kernel module settings
      extraModprobeConfig = ''
        # exclusive_caps: Skype, Zoom, Teams etc. will only show device when actually streaming
        # card_label: Name of virtual camera, how it'll show up in Skype, Zoom, Teams
        # https://github.com/umlaeute/v4l2loopback
        options v4l2loopback exclusive_caps=1 card_label="Virtual Camera"
      '';
    };
    environment.systemPackages = with pkgs; [ v4l-utils ];
  });
}
