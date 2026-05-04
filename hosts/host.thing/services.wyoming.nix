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
  # --- Wyoming Faster Whisper ---
  services.wyoming.faster-whisper.servers.main = {
    enable = false; # Temporarily disabled
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

  # --- Wyoming Piper TTS ---
  services.wyoming.piper.servers.yoda = {
    enable = false; # Temporarily disabled
    voice = "en-us-ryan-high";
    uri = "tcp://0.0.0.0:10200";
    useCUDA = true;
  };

  # --- Wyoming OpenWakeWord ---
  services.wyoming.openwakeword = {
    enable = false; # Temporarily disabled
    uri = "tcp://0.0.0.0:10400";
  };
}
