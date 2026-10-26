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
        llmops = {
          enable = true;
          # searx.enable = true;
          inference-cpp = {
            enable = true;
          };
          # open-webui = {
          #   enable = true;
          # };
        };
        dev = {
          enable = true;
          opencode.enable = true;
          pi-coding-agent = {
            enable = true;
            litellmUrl = "http://localhost:4000";
          };

          # The `mysbx` sandboxing CLI (modules/myconfig.ai.dev/mysbx/README.md).
          # Like the other sandbox tiers it is enabled EXPLICITLY per host and
          # never implicitly through the broad `myconfig.ai.llmops.enable`; it only
          # puts the CLI on PATH.
          mysbx.enable = true;
          # The heavy backend this host runs (bd myconfig-dq2w): p14
          # carries the podman-gvisor tier
          # (docs/feature-comparison.md); the other heavy backends stay
          # off — the flags gate the closure inputs.
          mysbx.podman.enable = true;
        };
      };
    };
    networking.firewall.interfaces."wg0".allowedTCPPorts = [ 443 ];
  };
}
