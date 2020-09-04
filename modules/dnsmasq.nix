# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:

{
  services.dnsmasq = {
    enable = true;
    extraConfig = ''
      conf-dir=/etc/dnsmasq.d
    '';
  };
}
