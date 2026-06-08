# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  lib,
  config,
  ...
}:
let
  headroom = {
    image = "ghcr.io/chopratejas/headroom:latest";

    ports = [ "127.0.0.1:8787:8787" ];

    extraOptions = [
      "--pull=always"
      "--name=headroom"
      "--hostname=headroom"
    ];
  };
in
{
  options.myconfig = with lib; {
    ai.container.headroom = {
      enable = mkEnableOption "myconfig.ai.container.headroom";
    };
  };
  config = lib.mkIf (config.myconfig.ai.enable && config.myconfig.ai.container.headroom.enable) {
    virtualisation.oci-containers.containers = { inherit headroom; };
  };
}
