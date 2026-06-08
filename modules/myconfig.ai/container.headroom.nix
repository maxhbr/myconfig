# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.myconfig.ai.container.headroom;

  headroom = {
    image = "ghcr.io/chopratejas/headroom:latest";

    ports = [ "127.0.0.1:8787:8787" ];

    extraOptions = [
      "--pull=always"
      "--name=headroom"
      "--hostname=headroom"
      "--env=HEADROOM_TELEMETRY=off"
      "--env=OPENAI_TARGET_API_URL=${cfg.targetApiUrl}"
    ];
  };
in
{
  options.myconfig = with lib; {
    ai.container.headroom = {
      enable = mkEnableOption "myconfig.ai.container.headroom";
      targetApiUrl = mkOption {
        type = types.str;
        description = "The OpenAI-compatible API URL that headroom targets.";
      };
    };
  };
  config = lib.mkIf (config.myconfig.ai.enable && config.myconfig.ai.container.headroom.enable) {
    virtualisation.oci-containers.containers = { inherit headroom; };
  };
}
