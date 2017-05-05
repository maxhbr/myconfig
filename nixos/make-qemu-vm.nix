#!/usr/bin/env nix-build
# build an ISO image that will auto install NixOS and reboot
# $ nix-build make-iso.nix
# delete with
# $ nix-store --delete <result>
#
# stolen from https://github.com/snabblab/snabblab-nixos/blob/master/make-iso.nix

let
  config = (import <nixpkgs/nixos/lib/eval-config.nix> {
    system = "x86_64-linux";
    modules = [
      <nixpkgs/nixos/modules/virtualisation/qemu-vm.nix>
      ({ pkgs, lib, ... }:
      let
        baseConfig = {
          nixpkgs.config = import ../nix/nixpkgs-config.nix;
        };
  
        machineConfig = import ./machines {
          config = baseConfig;
          hostId = "12ABCDEF";
          hostName = "vm";
        };
  
      in {
        services.openssh.permitRootLogin = "yes";
        users.extraUsers.root.initialPassword = lib.mkForce "dummy";
        
        environment.etc = {
          myconfig.source = ../.;
        };
      } // machineConfig)
    ];
  
  }).config;
in
  config.system.build.vm
