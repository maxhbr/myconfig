# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:

{
  config = {
    boot = {
      # kernelModules = [ "fuse" "kvm-intel" "coretemp" ];
      cleanTmpDir = true;
      # tmpOnTmpfs = true;
      crashDump.enable = true;
    };

    networking = {
      networkmanager.enable = true;
      firewall = {
        enable = true;
        allowPing = false;
      };
    };

    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        ag
      ];
      home.file = {
        ".agignore" = {
          text = ''
            /.git/
          '';
        };
      };
    };

    environment = {
      variables = {
        TMP = "/tmp";
      };
      interactiveShellInit = ''
        alias upg='~/myconfig/rebuild.sh'
        alias myborgbackup.sh=${./bin/myborgbackup.sh}
      '';
      # shellInit = ''
      # '';
      # loginShellInit = ''
      # '';
      systemPackages = with pkgs; [
        kbd

        # core:
        wget curl
        git git-lfs
        unzip
        tree
        stow
        rlwrap

        # cli:
        ranger
        vim
        elinks w3m
        tmux
        manpages
        # taskwarrior
        pass gopass
        file

        # admin:
        htop iftop iptraf-ng iotop bmon s-tui
        mtr bind bridge-utils
        pwgen # unstable.mkpasswd
        usbutils
        sysstat
        tcpdump
        cryptsetup
        lsof
        psmisc # contains: killall, pstree
        lm_sensors

        #others:
        pmount fuse
        rsnapshot
        mosh

        # my backup tool
        borgbackup
      ];
    };

    nix = {
      useSandbox = true;
      readOnlyStore = true;

      trustedBinaryCaches = [ "https://cache.nixos.org" ];
      binaryCaches = [ "https://cache.nixos.org" ];

      extraOptions = ''
        gc-keep-outputs = true
        gc-keep-derivations = true
        auto-optimise-store = true
        binary-caches-parallel-connections = 10
      '';
    };

    system.activationScripts.media = ''
      mkdir -m 0755 -p /media
    '';

    services = {
      nixosManual.showManual = true;
      acpid.enable = true;
      ntp.enable = true;
      nscd.enable = true;
    };

    security = {
      sudo.extraConfig = ''
        ALL  ALL=(ALL) NOPASSWD: /run/current-system/sw/bin/systemctl suspend
        ALL  ALL=(ALL) NOPASSWD: /run/current-system/sw/bin/systemctl reboot
        ALL  ALL=(ALL) NOPASSWD: /run/current-system/sw/bin/systemctl poweroff
      '';
      wrappers = {
        pmount.source  = "${pkgs.pmount}/bin/pmount";
        pumount.source  = "${pkgs.pmount}/bin/pumount";
      };
    };
    programs.ssh.startAgent = true;
  };
}
