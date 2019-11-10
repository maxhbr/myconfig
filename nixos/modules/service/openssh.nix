# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:

{
  config =  {
    services.openssh = {
      enable = true;
      passwordAuthentication = false;
      forwardX11 = true;
    };
  };
}
