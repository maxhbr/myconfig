# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, ... }:
{
  # udevd restarts (triggered by udev rule changes on NixOS switches) make
  # NetworkManager re-evaluate its devices and drop connections. Reload
  # rules instead of restarting the daemon.
  config = lib.mkIf config.services.udev.enable {
    systemd.services.systemd-udevd = {
      reloadIfChanged = true;
      serviceConfig.ExecReload = "${lib.getBin config.systemd.package}/bin/udevadm control --reload";
    };
  };
}
