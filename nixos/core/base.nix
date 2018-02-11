{ config, pkgs, lib, ... }:

{
  imports = [
    ./mhuber.nix
    ./oh-my-zsh.nix
    ./hosts.nix
  ];

  nixpkgs.config.permittedInsecurePackages = [
    "linux-4.13.16"
  ];
  boot = {
    kernelPackages = let
      versionLatest = lib.getVersion pkgs.linuxPackages_latest.kernel;
      version4_13 = lib.getVersion pkgs.linuxPackages_4_13.kernel;
      latestIsNewer =  lib.versionOlder version4_13 versionLatest;
      in if latestIsNewer
         then pkgs.linuxPackages_latest
         else pkgs.linuxPackages_4_13;
    # kernelPackages = pkgs.linuxPackages_latest;
    # kernelPackages = pkgs.linuxPackages_testing;
    # kernelPackages = unstable.linuxPackages_latest;
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
      unstable.git unstable.git-lfs
      unzip
      unstable.nox
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
      unstable.htop unstable.iftop unstable.iptraf-ng unstable.iotop unstable.bmon
      unstable.mtr  unstable.bind bridge-utils
      unstable.mkpasswd unstable.pwgen
      usbutils
      sysstat
      tcpdump
      cryptsetup
      lsof
      #others:
      pmount fuse
      rsnapshot
      # my backup tool
      borgbackup
    ];
    variables = { TMP = "/tmp"; };
  };

  nixpkgs.overlays = [(
    self: super: {
      pass = super.pass.overrideDerivation ( drv: {
        # should work for 1.7.1
        patches = drv.patches ++ [ ./patches/pass_-_copy_by_default.diff ];
      });
    }
  )];

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

  system = {
    activationScripts.media = ''
      mkdir -m 0755 -p /media /share
    '';
  };

  services = {
    nixosManual.showManual = true;
    acpid.enable = true;
    ntp.enable = true;
    emacs = {
      enable = true;
      install = true;
      defaultEditor = true;
      package = pkgs.unstable.emacs;
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
