# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/virtualisation/amazon-config.nix>
    ../profiles/terminal.nix
  ];
}
