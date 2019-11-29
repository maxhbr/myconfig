# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:

{
  imports = [
    ./dev/jdk.nix
    ./dnsmasq.nix
  ];
  environment = {
    systemPackages = with pkgs; [
      openvpn networkmanager_openvpn
      openconnect networkmanager-openconnect
      strongswan networkmanager_strongswan
      libreoffice
      zoom-us
      rambox
      p7zip
      thrift011
      idea-ultimate jetbrains.phpstorm
      dia
    ];
  };
}
