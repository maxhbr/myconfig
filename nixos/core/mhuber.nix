# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:

let
  hasVBox = config.virtualisation.virtualbox.host.enable;
  hasDocker = config.virtualisation.docker.enable;
  hasNM = config.networking.networkmanager.enable;
  hasBB = config.hardware.bumblebee.enable;

in {
  users = {
    mutableUsers = true; # one needs to change the password?
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
        ++ pkgs.lib.optional hasNM "networkmanager"
        ++ pkgs.lib.optional hasVBox "vboxusers"
        ++ pkgs.lib.optional hasDocker "docker"
        ++ pkgs.lib.optional hasBB "bumblebee";
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

  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "neo";
    defaultLocale = "de_DE.UTF-8";
    supportedLocales = ["de_DE.UTF-8/UTF-8" "en_US.UTF-8/UTF-8"];
  };
}
