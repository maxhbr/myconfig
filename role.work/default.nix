# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }: {
  imports = [
    ../role.dev
    ./jdk.nix
    ./misc-desktop-tools.nix
    ../secrets/common/wifi.TNG.nix
  ];
  config = {
    nixpkgs.overlays = map (n: import n) [
      ./idea-ultimate
      ./thrift011.nix
      ./thrift012.nix
      ./thrift93.nix
    ];
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        openvpn
        networkmanager_openvpn
        openconnect
        networkmanager-openconnect
        strongswan
        networkmanager_strongswan
        networkmanagerapplet
        thrift
        idea-ultimate # jetbrains.phpstorm
        dia
        insync
      ];
    };
  };
}
