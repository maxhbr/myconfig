# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
let
  myPorts = with pkgs; writeScriptBin "myPorts" ''
    #!${stdenv.shell}
    /run/wrappers/bin/sudo ${iproute}/bin/ss -tulpen
  '';
in {
  imports = [
    ./dnsmasq.nix
    ./extrahosts
  ];
  config = {
    environment.systemPackages = with pkgs; [
      myPorts
    ];
    networking = {
      networkmanager.enable = true;
      firewall = {
        enable = true;
        allowPing = false;
      };
    };
  };
}
