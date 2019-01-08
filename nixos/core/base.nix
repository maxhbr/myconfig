# Copyright 2017-2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:

{
  imports = [
    ./mhuber.nix
    ./oh-my-zsh.nix
    ./hosts.nix
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    kernelModules = [ "fuse" "kvm-intel" "coretemp" ];
    cleanTmpDir = true;
    # tmpOnTmpfs = true;
  };

  networking = {
    networkmanager.enable = true;
    firewall = {
      enable = true;
      # allowedTCPPorts = [ 80 443 ];
      allowPing = false;
    };
  };

  environment = {
    variables = {
      TMP = "/tmp";
      BROWSER = "${pkgs.chromium}/bin/chromium-browser";
      EDITOR = "${pkgs.myconfig.scripts}/bin/ec -t";
    };
    interactiveShellInit = ''
      alias upg='~/myconfig/rebuild.sh'
      alias vim="${pkgs.myconfig.scripts}/bin/ec -t"
      alias emacs="${pkgs.myconfig.scripts}/bin/ec"
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
      emacs vim
      elinks w3m
      tmux
      manpages
      taskwarrior
      pass
      ag
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

      #others:
      pmount fuse
      rsnapshot

      # my backup tool
      borgbackup
    ];
  };

  nix = {
    useSandbox = true;
    readOnlyStore = true;

    binaryCachePublicKeys = [
       "hydra.snabb.co-1:zPzKSJ1mynGtYEVbUR0QVZf9TLcaygz/OyzHlWo5AMM=" # snabb.co
    ];
    trustedBinaryCaches = [
      "https://cache.nixos.org" "https://hydra.snabb.co"
    ];
    binaryCaches = [
      "https://cache.nixos.org" "https://hydra.snabb.co"
    ];

    extraOptions = ''
      gc-keep-outputs = true
      gc-keep-derivations = true
      auto-optimise-store = true
      binary-caches-parallel-connections = 10
    '';
  };

  system.activationScripts.media = ''
    mkdir -m 0755 -p /media /share
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
}
