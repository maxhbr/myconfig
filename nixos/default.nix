# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

let
  # echo -n "HOSTNAME" | sudo tee /etc/nixos/hostname
  hostName = "${builtins.readFile /etc/nixos/hostname}";
  # cksum /etc/machine-id | while read c rest; do printf "%x" $c; done | sudo tee /etc/nixos/hostid
  hostId = "${builtins.readFile /etc/nixos/hostid}";
in {
  networking.hostId = "${hostId}";
  networking.hostName = "${hostName}";
  system.copySystemConfiguration = true;

  boot.kernel.sysctl = {
    # "fs.inotify.max_user_watches" = 524288;
    "vm.swappiness" = 1;
  };

  nixpkgs = {
    config = import ../nix/nixpkgs-config.nix;
    overlays = import ../nix/nixpkgs-overlays.nix;
  };

  users= {
    extraUsers.myconfig = {
      isNormalUser = false;
      group = "myconfig";
      uid = 999;
      home = "/home/myconfig";
      createHome = true;
    };
    extraGroups.myconfig.gid = 999;
  };

  imports = [
    /etc/nixos/hardware-configuration.nix
    ./core
    ./roles
  ]
  # the machine specific configuration is placed at ./machines/<hostName>.nix
    ++ (let
          path = (./machines + "/${hostName}.nix");
        in if builtins.pathExists path
           then [path]
           else [])
  # old configuration can be placed at /etc/nixos/configuration.old.nix
    ++ (if builtins.pathExists /etc/nixos/configuration.old.nix
        then [/etc/nixos/configuration.old.nix]
        else [])
  # all files in /etc/nixos/imports are sourced
    ++ (let
          path = /etc/nixos/imports;
        in if builtins.pathExists path
           then let
                  content = builtins.readDir path;
                in map (n: import (path + ("/" + n)))
                         (builtins.filter (n: builtins.match ".*\\.nix" n != null || builtins.pathExists (path + ("/" + n + "/default.nix")))
                           (builtins.attrNames content))
           else [])
  # all files in ./misc are sourced
    ++ (let
          path = ./misc;
        in if builtins.pathExists path
           then let
                  content = builtins.readDir path;
                in map (n: import (path + ("/" + n)))
                         (builtins.filter (n: builtins.match ".*\\.nix" n != null || builtins.pathExists (path + ("/" + n + "/default.nix")))
                           (builtins.attrNames content))
           else []);
}
