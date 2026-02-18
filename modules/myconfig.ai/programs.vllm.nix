# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  ...
}:
let
  vllm = with pkgs; if (config.myconfig.hardware.gpu.variant == "amd")
    then python3Packages.vllm.override { 
      rocmSupport = true;
      cudaSupport = false;
    }
    else python3Packages.vllm;
in 
{
  options.myconfig = with lib; {
    ai.vllm = {
      enable = mkEnableOption "myconfig.ai.vllm";
      package = mkOption {
        type = types.package;
        default = vllm;
        description = "vllm package to use";
      };
    };
  };
  config = lib.mkIf config.myconfig.ai.vllm.enable {
    home-manager.sharedModules = [
      {
        home.packages = [ config.myconfig.ai.vllm.package ];
      }
    ];
  };
}
