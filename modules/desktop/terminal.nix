# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:

{
  imports = [
    ../openssh.nix
  ];
  config = {
    import <nixpkgs/nixos/modules/services/x11/terminal-server.nix> {
    inherit config lib pkgs;
  });
}
