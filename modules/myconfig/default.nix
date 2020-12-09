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
  };
  imports = [
    ./myconfig.desktop.nix
    ./myconfig.headless.nix
    ./myconfig.virtualisation.nix
  ];
}
