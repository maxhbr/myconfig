{ config, pkgs, lib, unstable, ... }:

{
  imports = [
    ./oh-my-zsh.nix
    ./mhuber.nix
  ];

  boot = {
    # kernelPackages = pkgs.linuxPackages_testing;
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
      nox
      tree
      # cli:
      ranger
      emacs vim
      elinks w3m
      tmux
      manpages
      taskwarrior
      pass
      # admin:
      htop
      iftop iptraf-ng iotop mtr bind
      mkpasswd pwgen
      usbutils
      #others:
      pmount fuse
      cryptsetup
      rsnapshot
      stow
    ];
    variables = { TMP = "/tmp"; };
  };

  nix = {
    useSandbox = true;
    readOnlyStore = true;

    binaryCachePublicKeys = [
       "hydra.cryp.to-1:8g6Hxvnp/O//5Q1bjjMTd5RO8ztTsG8DKPOAg9ANr2g=" # crypt.to
       "hydra.snabb.co-1:zPzKSJ1mynGtYEVbUR0QVZf9TLcaygz/OyzHlWo5AMM=" # snabb.co
    ];
    trustedBinaryCaches = [
      "https://cache.nixos.org" "https://hydra.snabb.co" "http://hydra.cryp.to/"
    ];
    binaryCaches = [
      "https://cache.nixos.org" "https://hydra.snabb.co" "http://hydra.cryp.to/"
    ];

    extraOptions = ''
      gc-keep-outputs = true
      gc-keep-derivations = true
      auto-optimise-store = true
      binary-caches-parallel-connections = 10
    '';
  };

  system = {
    activationScripts.media = ''
      mkdir -m 0755 -p /media /share
    '';
  };

  services = {
    nixosManual.showManual = true;
    acpid.enable = true;
    ntp.enable = true;
    # emacs = {
    #   enable = true;
    #   # package = import /home/mhuber/.emacs.d { pkgs = pkgs; };
    # };
  };

  security.wrappers = {
    pmount.source  = "${pkgs.pmount}/bin/pmount";
    pumount.source  = "${pkgs.pmount}/bin/pumount";
  };
  programs.ssh.startAgent = true;
}
