# see:
# - https://github.com/NixOS/nixpkgs/pull/85690
# - https://github.com/colemickens/nixcfg
{ pkgs, config, lib, ... }:
let cfg = config.myconfig;
in {
  options.myconfig = with lib; { v4l2.enable = mkEnableOption "v4l2"; };

  imports = [
    # # v4l2loopback currently fails to compile:
    # ( { pkgs, config, lib, ... }:
    #   (lib.mkIf cfg.v4l2.enable {
    #     extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];
    #     kernelModules = [
    #       "v4l2loopback"
    #     ]
    #     # Set initial kernel module settings
    #     extraModprobeConfig = ''
    #       # exclusive_caps: Skype, Zoom, Teams etc. will only show device when actually streaming
    #       # card_label: Name of virtual camera, how it'll show up in Skype, Zoom, Teams
    #       # https://github.com/umlaeute/v4l2loopback
    #       options v4l2loopback exclusive_caps=1 card_label="Virtual Camera"
    #     '';
    #   })
  ];

  config = (lib.mkIf cfg.v4l2.enable {
    boot = {
      kernelModules = [
        # Virtual Microphone, built-in
        "snd-aloop"
      ];
    };
    environment.systemPackages = with pkgs; [ v4l-utils ];
  });
}
