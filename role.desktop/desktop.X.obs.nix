# see:
# - https://github.com/NixOS/nixpkgs/pull/85690
# - https://github.com/colemickens/nixcfg
{ config, pkgs, ... }:
let user = config.myconfig.user;
in {
  home-manager.users."${user}" = {
    programs.obs-studio = {
      enable = true;
      plugins = with pkgs;
        [
          obs-v4l2sink
          #obs-wlrobs
        ];
    };
  };
  boot.extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];
  environment.systemPackages = with pkgs; [ v4l-utils ];
}
