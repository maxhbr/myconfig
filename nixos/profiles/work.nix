{ config, pkgs, ... }:

let
  unstable = (import (fetchTarball http://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) {});
in {
  environment.systemPackages = with pkgs; [
    openvpn networkmanager_openvpn
    rdesktop
    # citrix_receiver
    openjdk unstable.maven thrift gradle
    libreoffice
    idea-ultimate
  ];
  nixpkgs.overlays = [(import ../../nix/overlays/idea-ultimate.nix)];
}
