# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:

{
  config = {
    services.ipfs = {
      enable = true;
      swarmAddress = [ "/ip4/0.0.0.0/tcp/5000" "/ip6/::/tcp/5000" ];
      gatewayAddress = "/ip4/0.0.0.0/tcp/8080";
      autoMount = true;
      enableGC = true;
    };
  };
}
