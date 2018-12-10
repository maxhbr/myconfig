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
      unstable.git unstable.git-lfs
      unzip
      # unstable.nox # no longer works for my setup
      tree
      stow
      rlwrap
      # cli:
      ranger
      unstable.emacs unstable.vim
      unstable.elinks unstable.w3m
      unstable.tmux
      manpages
      taskwarrior
      pass
      unstable.ag
      file
      # admin:
      unstable.htop unstable.iftop unstable.iptraf-ng unstable.iotop unstable.bmon s-tui
      unstable.mtr unstable.bind bridge-utils
      unstable.pwgen # unstable.mkpasswd
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
    emacs = {
      enable = true;
      install = true;
      defaultEditor = true;
      package = pkgs.emacs;
      # package = import /home/mhuber/.emacs.d { pkgs = pkgs; };
    };
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
