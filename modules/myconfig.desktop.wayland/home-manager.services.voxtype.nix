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
        # State file for Waybar/polybar integration. The daemon writes
        # its current state (idle/recording/transcribing) here; the
        # waybar custom/voxtype module reads it via `voxtype status`.
        state_file = "auto";
        # Push-to-talk via the ^ key (below ESC). keyd remaps that physical
        # key (grave → f13) on its virtual device (see
        # myconfig.desktop.wayland.niri); voxtype reads evdev for F13 and
        # starts recording on press, stops + transcribes on release. evdev
        # read access comes from the `input` group (see nixos.user.nix).
        # NB: this requires keyd to be running (it is, on niri hosts); on a
        # host without keyd nothing emits F13 and the hotkey is inert.
        hotkey = {
          enabled = true;
          key = "F13";
          mode = "push_to_talk";
        };
        audio = {
          device = "default";
          sample_rate = 16000;
          max_duration_secs = 60;
          feedback = {
            enabled = true;
            theme = "subtle";
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
        meeting = {
          # Enable `voxtype meeting start/stop` for continuous
          # transcription with chunked processing and export.
          # Meetings are stored under ~/.local/share/voxtype/meetings/
          # (already persisted via cache-directories).
          enabled = true;
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
