#!/usr/bin/env nix-build
# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# build an ISO image that will auto install NixOS and reboot
# $ nix-build make-qemu-vm.nix --arg machine \"mobile\"
# delete with
# $ nix-store --delete <result>
#
# stolen from https://github.com/snabblab/snabblab-nixos/blob/master/make-iso.nix

{ machine ? "vm" }:

let
  config = (import <nixpkgs/nixos/lib/eval-config.nix> {
    system = "x86_64-linux";
    modules = [
      <nixpkgs/nixos/modules/virtualisation/qemu-vm.nix>
      ({ pkgs, lib, ... }: import ./common-make.nix {
        inherit pkgs lib machine;
      } // {
        virtualisation.memorySize = 1024;
        virtualisation.diskSize = 1024;
        virtualisation.graphics = false;
        services.mingetty.autologinUser = "mhuber";
        programs.zsh.shellInit = "touch ~/.zshrc";
        security.sudo.wheelNeedsPassword = false;
      })
    ];
  }).config;
in
  config.system.build.vm
