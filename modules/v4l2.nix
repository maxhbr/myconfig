# see:
# - https://github.com/NixOS/nixpkgs/pull/85690
# - https://github.com/colemickens/nixcfg
{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig;
in
{
  options.myconfig = with lib; {
    v4l2.enable = mkEnableOption "v4l2";
  };

  imports = [
    (
      {
        pkgs,
        config,
        lib,
        ...
      }:
      (lib.mkIf cfg.v4l2.enable {
        boot = {
        };
      })
    )
  ];

  config = (
    lib.mkIf cfg.v4l2.enable {
      boot = {
        extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];
        # Set initial kernel module settings
        kernelModules = [
          "snd-aloop" # Virtual Microphone, built-in
          "v4l2loopback"
        ];
        extraModprobeConfig = ''
          # exclusive_caps: Skype, Zoom, Teams etc. will only show device when actually streaming
          # card_label: Name of virtual camera, how it'll show up in Skype, Zoom, Teams
          # https://github.com/umlaeute/v4l2loopback
          # options v4l2loopback exclusive_caps=1 card_label="Virtual Camera"
          options v4l2loopback devices=1 video_nr=1 card_label="OBS Cam" exclusive_caps=1
        '';
      };
      security.polkit.enable = true;
      environment.systemPackages = with pkgs; [ v4l-utils ];
    }
  );
}
