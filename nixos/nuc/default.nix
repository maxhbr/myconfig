# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ../headless
    # hardware:
    ../hardware/efi.nix
  ];
  config = {
    networking.wireless.enable = true;
  };
}
