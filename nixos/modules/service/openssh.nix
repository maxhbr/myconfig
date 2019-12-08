# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, ... }:

{
  config =  {
    services.openssh = {
      enable = true;
      passwordAuthentication = false;
      forwardX11 = config.services.xserver.enable;
    };
  };
}
