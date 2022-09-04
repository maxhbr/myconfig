# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, myconfig, ... }: {
  config = {
    users = {
      mutableUsers = false;
      extraUsers = {
        "${myconfig.user}" = {
          isNormalUser = true;
          group = "${myconfig.user}";
          uid = 1000;
          extraGroups =
            [ "myconfig" "wheel" "keys" "audio" "video" "dialout" "input" ]
            ++ pkgs.lib.optional config.virtualisation.virtualbox.host.enable
            "vboxusers"
            ++ pkgs.lib.optional config.virtualisation.docker.enable "docker"
            ++ pkgs.lib.optional config.virtualisation.libvirtd.enable "kvm"
            ++ pkgs.lib.optional config.virtualisation.libvirtd.enable "libvirt"
            ++ pkgs.lib.optional config.virtualisation.lxc.enable "lxc"
            ++ pkgs.lib.optional config.virtualisation.lxd.enable "lxd"
            ++ pkgs.lib.optional config.networking.networkmanager.enable
            "networkmanager"
            ++ pkgs.lib.optional config.hardware.bumblebee.enable "bumblebee"
            ++ pkgs.lib.optional config.programs.sway.enable "sway"
            ++ pkgs.lib.optional config.services.pipewire.enable "realtime";
          home = "/home/${myconfig.user}";
          createHome = true;
          shell = "/run/current-system/sw/bin/fish";
          # the hashed password is overwritten in the deployment
          # default value is: "dummy"
          # can be generated with: mkpasswd -m sha-512
          hashedPassword = lib.mkDefault
            "$6$Yps/cy4g4$btJtv/9stI7vqmfgI87XRnT2V2hGv2iyDJHGPsm3T519rgbrwLxLkJEkQoO7mBre.qy6WIcjlJ/RM472SgcDE1";
          openssh.authorizedKeys.keys = [
            "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDYUWsgqFqIHd3ftOC1W4j24wa1C4gjVt8yzdlqKn0KovLH5e4TdnMMkcoyVWAmrR/fxLCX0XbbUUAlwpA5mwxnyd5vBO7P+6tm9Z4I7rGW4EDxgRVA45/4qbe1DOW4qkYGf0MLidzq7xMXS7UIGcMZk1K0EIhj8tL1fPrPa8cV94/x5PAOGwXSALG3RFLH+8xxCOtean0/1Ev9+l+W19cp8+SwksjMCCbj1yrIgLPMaclMerZ6oUAKB6yXYkAoSSfVvSQLp9iRWwKkOAjDPvWCkKr4ICTKEY0z3lbhd587NwWmEUDAp9Z9rTNT5MVfKOGtlLVBMCB2rf0wmjuL/hlp"
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF4/Erg9yplOhyzAWSbzBcHTduXnMgQ5ToRniCKlUDYK"
          ];
        };
        root = {
          openssh.authorizedKeys.keys =
            config.users.extraUsers."${myconfig.user}".openssh.authorizedKeys.keys;
        };
      };
      extraGroups."${myconfig.user}".gid = 1000;
    };
    home-manager.users."${myconfig.user}" = {
      programs.alacritty.settings.shell.program =
        lib.mkForce config.users.extraUsers."${myconfig.user}".shell;
      xdg = {
        enable = true;
        mime.enable = true;
        configFile."mimeapps.list".force = true;
        mimeApps.enable = true;
      };
    };
    home-manager.sharedModules = [{ home.sessionPath = [ "~/bin" ]; }];

    services.xserver = (lib.mkIf config.services.xserver.enable {
      layout = "de";
      xkbVariant = "neo";
      xkbOptions = "altwin:swap_alt_win";
    });

    # environment.etc."current-home-manager-${myconfig.user}-packages".text = let
    #   packages = builtins.map (p: "${p.name}")
    #     config.home-manager.users."${myconfig.user}".home.packages;
    #   sortedUnique = builtins.sort builtins.lessThan (lib.unique packages);
    #   formatted = builtins.concatStringsSep "\n" sortedUnique;
    # in formatted;

    systemd.tmpfiles.rules = [
      "d /home/${myconfig.user}/tmp 1777 ${myconfig.user} ${myconfig.user} 10d"
    ];

    time.timeZone = "Europe/Berlin";

    location.latitude = 48.2;
    location.longitude = 10.8;

    console.font = "lat9w-16";
    console.keyMap = "neo";
    i18n = {
      defaultLocale = "de_DE.UTF-8";
      # defaultLocale = "en_us.UTF-8";
      supportedLocales = [ "de_DE.UTF-8/UTF-8" "en_US.UTF-8/UTF-8" ];
    };
  };
}
