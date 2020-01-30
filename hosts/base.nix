# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./minimal.nix
    # configuration
    ../profiles/desktop
    ../profiles/mail
    ../profiles/dev
  ];
}
