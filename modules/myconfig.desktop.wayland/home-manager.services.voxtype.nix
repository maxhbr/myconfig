# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  ...
}:

{
  config = lib.mkIf config.services.voxtype.enable {
    services.voxtype = {
      # Use the Vulkan variant for GPU-accelerated Whisper inference.
      package = pkgs.voxtype-vulkan;
      loadModels = [ "base" ];
      settings = {
        audio = {
          device = "default";
          sample_rate = 16000;
          max_duration_secs = 60;
          feedback = {
            enabled = true;
            theme = "default"; # built-in themes: default, subtle, mechanical
            volume = 0.7; # 0.0 to 1.0
          };
        };
        whisper = {
          model = "base";
          language = [
            "en"
            "de"
          ];
        };
        output = {
          mode = "type";
          fallback_to_clipboard = true;
        };
      };
    };

    myconfig.desktop.wayland.niri.extraBinds = ''
      Mod+G hotkey-overlay-title="Voxtype (voice)" { spawn "voxtype" "record" "toggle"; }
    '';
  };
}
