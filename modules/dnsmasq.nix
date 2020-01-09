# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:

{
  services.dnsmasq = {
    enable = true;
    servers = [ "8.8.8.8" "8.8.4.4" ];
    extraConfig = ''
conf-dir=/etc/dnsmasq.d
'';
  };
}
