# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Default configuration for the Voxtype push-to-talk speech-to-text daemon.
# Upstream home-manager option reference:
#   https://github.com/nix-community/home-manager/blob/master/modules/services/voxtype.nix
#
# The daemon types transcribed text at the cursor position over Wayland.
# `WAYLAND_DISPLAY` is imported into the systemd user environment by the
# compositor session (see `services.dbus` and `myconfig.desktop.wayland.niri`),
# so `services.voxtype.wayland.display` is left at its `null` default instead
# of hardcoding a socket name that varies per session.
{
  config,
  lib,
  pkgs,
  ...
}:

{
  config = lib.mkIf config.services.voxtype.enable {
    services.voxtype = {
      # Pre-download the configured Whisper model so the daemon can transcribe
      # on first use without a network round-trip. Must match `whisper.model`.
      loadModels = [ "base.en" ];

      settings = {
        audio = {
          device = "default";
          sample_rate = 16000;
          max_duration_secs = 60;
        };
        whisper = {
          model = "base.en";
          language = "en";
        };
        output = {
          mode = "type";
          fallback_to_clipboard = true;
        };
      };
    };

    # Niri push-to-toggle keybind for voxtype. Niri has no key-release binds,
    # so push-to-talk uses `voxtype record toggle` (press to start recording,
    # press again to transcribe and type at the cursor). Injected into the
    # niri `binds { }` block via the `@@niri-extra-binds@@` marker handled by
    # modules/myconfig.desktop.wayland.niri.
    myconfig.desktop.wayland.niri.extraBinds = ''
      Mod+G hotkey-overlay-title="Voxtype (voice)" { spawn "voxtype" "record" "toggle"; }
    '';
  };
}
