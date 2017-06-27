{ config, pkgs, ... }:

let
  unstable = (import <unstable> {});
in {
  environment.systemPackages = with pkgs; [
    openvpn networkmanager_openvpn
    rdesktop
    # citrix_receiver
    openjdk unstable.maven thrift gradle
    libreoffice
    idea.idea-ultimate
  ];
  nixpkgs.overlays = [(import ../../nix/overlays/idea-ultimate.nix)];
}
