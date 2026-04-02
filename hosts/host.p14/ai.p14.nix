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
        inference-cpp = {
          enable = true;
        };
        open-webui = {
          enable = true;
        };
      };
    };
    networking.firewall.interfaces."wg0".allowedTCPPorts = [ 443 ];
  };
}
