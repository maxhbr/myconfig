# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:
{
  config = {
    users = {
      mutableUsers = true;
      extraUsers.mhuber = {
        isNormalUser = true;
        group = "mhuber";
        uid = 1000;
        extraGroups = [
          "myconfig"
          "wheel"
          "audio" "video"
          "dialout"
          "input" ]
          ++ pkgs.lib.optional config.virtualisation.virtualbox.host.enable "vboxusers"
          ++ pkgs.lib.optional config.virtualisation.docker.enable "docker"
          ++ pkgs.lib.optional config.virtualisation.libvirtd.enable "kvm"
          ++ pkgs.lib.optional config.networking.networkmanager.enable "networkmanager"
          ++ pkgs.lib.optional config.hardware.bumblebee.enable "bumblebee"
          ++ pkgs.lib.optional config.programs.sway.enable "sway"
          ;
        home = "/home/mhuber";
        createHome = true;
        shell = "/run/current-system/sw/bin/zsh";
        # password = "dummy";
        initialPassword = lib.mkForce "dummy";
        openssh.authorizedKeys.keys = [
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDYUWsgqFqIHd3ftOC1W4j24wa1C4gjVt8yzdlqKn0KovLH5e4TdnMMkcoyVWAmrR/fxLCX0XbbUUAlwpA5mwxnyd5vBO7P+6tm9Z4I7rGW4EDxgRVA45/4qbe1DOW4qkYGf0MLidzq7xMXS7UIGcMZk1K0EIhj8tL1fPrPa8cV94/x5PAOGwXSALG3RFLH+8xxCOtean0/1Ev9+l+W19cp8+SwksjMCCbj1yrIgLPMaclMerZ6oUAKB6yXYkAoSSfVvSQLp9iRWwKkOAjDPvWCkKr4ICTKEY0z3lbhd587NwWmEUDAp9Z9rTNT5MVfKOGtlLVBMCB2rf0wmjuL/hlp"
        ];
      };
      extraGroups.mhuber.gid = 1000;
    };
    systemd.tmpfiles.rules = [ "d /home/mhuber/tmp 1777 mhuber mhuber 10d" ];

    time.timeZone = "Europe/Berlin";

    location.latitude = 48.2;
    location.longitude = 10.8;

    console.font = "lat9w-16";
    console.keyMap = "neo";
    i18n = {
      defaultLocale = "de_DE.UTF-8";
      supportedLocales = ["de_DE.UTF-8/UTF-8" "en_US.UTF-8/UTF-8"];
    };

    home-manager.users.mhuber = {
      nixpkgs.config.allowUnfree = true;
    };
  };
}
