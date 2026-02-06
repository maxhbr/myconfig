# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.myconfig = with lib; {
    ai.vllm = {
      enable = mkEnableOption "myconfig.ai.vllm";
    };
  };
  config = lib.mkIf config.myconfig.ai.vllm.enable {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [ vllm ];
      }
    ];
  };
}
