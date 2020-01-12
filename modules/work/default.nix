# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:

{
  imports = [
    ./jdk.nix
    ../dev/common.nix
  ];
  config = {
    nixpkgs.overlays = map (n: import n) [
      ./idea-ultimate
      ./thrift011.nix
      ./thrift012.nix
      ./thrift93.nix
      ./zoom-us.nix
    ];
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        openvpn networkmanager_openvpn
        openconnect networkmanager-openconnect
        strongswan networkmanager_strongswan
        networkmanagerapplet
        libreoffice
        zoom-us
        rambox
        p7zip
        thrift011
        idea-ultimate # jetbrains.phpstorm
        dia
      ];
    };
  };
}
