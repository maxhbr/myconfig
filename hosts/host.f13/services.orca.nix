# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  ...
}:
{
  # Install the Orca package (AppImage + CLI wrappers) on f13, but do
  # NOT run the `orca serve` systemd service. See
  # modules/myconfig.ai/services.orca.nix for the enable / service split.
  myconfig.ai.orca.enable = true;
}
