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
      loadModels = [ "large-v3-turbo" ];
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
          model = "large-v3-turbo";
          language = [
            "en"
            "de"
          ];
        };
        output = {
          mode = "type";
          fallback_to_clipboard = true;
        };
        text = {
          # Custom word replacements applied after transcription
          # (case-insensitive, preserves word boundaries). Fix commonly
          # misheard words, brand names, and technical terms.
          replacements = {
            "vox type" = "voxtype";
            "oh marky" = "Omarchy";
          };
          # Convert spoken punctuation words into symbols — useful for
          # developers and technical writing (e.g. saying "open paren"
          # produces "(").
          spoken_punctuation = true;
        };
      };
    };

    myconfig.desktop.wayland.niri.extraBinds = ''
      Mod+G hotkey-overlay-title="Voxtype (voice)" { spawn "voxtype" "record" "toggle"; }
    '';

    # Persist downloaded whisper models across reboots so the
    # voxtype-model-loader service doesn't re-download them.
    myconfig.persistence.cache-directories = [ ".local/share/voxtype/" ];
  };
}
