# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
{
  imports = [
    # ../../hardware/eGPU.nix
  ];

  config = {
    myconfig = {
      ai = {
        enable = true;
        opencode.enable = true;
        pi-coding-agent.enable = true;
        # searx.enable = true;
        inference-cpp = {
          enable = true;
        };
        # open-webui = {
        #   enable = true;
        # };
        container = {
          headroom = {
            enable = true;
            targetApiUrl = "http://litellm.thing.wg0.maxhbr.local/v1";
          };
        };
      };
    };
    networking.firewall.interfaces."wg0".allowedTCPPorts = [ 443 ];
  };
}
