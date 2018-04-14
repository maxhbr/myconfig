#!/usr/bin/env nix-build
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# build an ISO image that will auto install NixOS and reboot
# $ nix-build make-iso.nix
# delete with
# $ nix-store --delete <result>
#
# stolen from https://github.com/snabblab/snabblab-nixos/blob/master/make-iso.nix

{ machine ? "iso" }:

let
  config = (import <nixpkgs/nixos/lib/eval-config.nix> {
    system = "x86_64-linux";
    modules = [
      <nixpkgs/nixos/modules/installer/cd-dvd/iso-image.nix>
      ({ pkgs, lib, ... }: import ./common-make.nix {
        inherit pkgs lib machine;
      } // {
        services.mingetty.autologinUser = "mhuber";
        security.sudo.wheelNeedsPassword = false;
      })
    ];
  }).config;
in
  config.system.build.isoImage
