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
    (lib.mkIf config.services.wyoming.faster-whisper.servers.main.enable {
      # --- Wyoming Faster Whisper ---
      services.wyoming.faster-whisper.servers.main = {
        model = "large-v3-turbo";
        language = "auto";
        device = "cuda";
        uri = "tcp://0.0.0.0:10300";
      };

      # --- Wyoming Faster Whisper Hardening ---
      # Auto-restart on failure (including OOM kills)
      systemd.services.wyoming-faster-whisper-main = {
        serviceConfig = {
          Restart = "on-failure";
          RestartSec = 10;
          # Memory limits to prevent system-wide OOM
          MemoryMax = "16G";
          MemoryHigh = "14G";
        };
      };
    })
    (lib.mkIf config.services.wyoming.piper.servers.yoda.enable {
      # --- Wyoming Piper TTS ---
      services.wyoming.piper.servers.yoda = {
        voice = "en-us-ryan-high";
        uri = "tcp://0.0.0.0:10200";
        useCUDA = true;
      };

    }
    (lib.mkIf config.services.wyoming.openwakeword.enable {
      # --- Wyoming OpenWakeWord ---
      services.wyoming.openwakeword = {
        uri = "tcp://0.0.0.0:10400";
      };
    })
  ];
  config = {
    services.wyoming.faster-whisper.servers.main.enable = false;
    services.wyoming.piper.servers.yoda.enable = false;
    services.wyoming.openwakeword.enable = false;
  };
}
