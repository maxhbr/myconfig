{ config, lib, pkgs, ... }:
let
  cfg = config.myconfig;
  user = cfg.user;
in {
  options.myconfig = with lib; {
    user = mkOption {
      type = types.str;
      default = "mhuber";
      example = "mhuber";
      description = ''
        The username of the main interactive user
      '';
    };
    # config = mkOption {
    #   type = types.attrs;
    #   default = {};
    # };
  };
  imports = [
    ./myconfig.desktop.nix
    ./myconfig.headless.nix
    ./myconfig.virtualisation.nix
    ./myconfig.virtualization.gpu-passthrough-host.nix
    ./myconfig.imagework.nix
  ];
}
