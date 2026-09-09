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
        dev = {
          opencode.enable = true;
          pi-coding-agent.enable = true;

          # The `mysbx` sandboxing CLI (modules/myconfig.ai.dev/mysbx/README.md).
          # Like the other sandbox tiers it is enabled EXPLICITLY per host and
          # never implicitly through the broad `myconfig.ai.enable`; it only
          # puts the CLI on PATH.
          mysbx.enable = true;

        };
        # searx.enable = true;
        inference-cpp = {
          enable = true;
        };
        # open-webui = {
        #   enable = true;
        # };
      };
    };
    networking.firewall.interfaces."wg0".allowedTCPPorts = [ 443 ];
  };
}
