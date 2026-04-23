# Copyright 2016-2025 Maximilian Huber <oss@maximilian-huber.de>
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
  config = lib.mkIf myconfig.ai.comfyui.enable {
    myconfig = {
      ai = {
        comfyui = {
          cuda_version = "cu129";
          rocm_version = "gfx1151";
          userservice = true;
        };
      };
    };
    home-manager.sharedModules = [
      {
        myconfig.persistence.cache-directories = [ ".config/comfy-ui/" ];
      }
    ];
  };
}
