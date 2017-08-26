{ config, lib, pkgs, ... }:

{
  options = {
    myconfig.roles.terminal = {
      enable = lib.mkEnableOption "Terminal role";
    };
  };

  config = lib.mkIf config.myconfig.roles.terminal.enable {
    myconfig.roles.openssh.enable = true;
  } // (import <nixpkgs/nixos/modules/services/x11/terminal-server.nix> {
    inherit config lib pkgs;
  });
}
