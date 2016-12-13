{ config, pkgs, ... }:

{

  boot = {
    # kernelPackages = pkgs.linuxPackages_testing;
    kernelModules = [ "fuse" "kvm-intel" "coretemp" ];
    cleanTmpDir = true;
 #  loader.grub = {
 #    enable = true;
 #    version = 2;
 #    device = "/dev/sda";
 #    memtest86.enable = true;
 #  };
    loader.systemd-boot.enable = true;
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
    systemPackages = with pkgs; [
      kbd
      wget curl elinks w3m
      git
      emacs vim
      tmux
      htop iftop powertop
      ranger
      pmount fuse usbutils
      acpi acpid
      mkpasswd
      manpages
      taskwarrior
      pass
      cryptsetup
      rsnapshot
    ];
    # shellAliases = {
    #   ll = "ls -l";
    # };
    shells = ["/run/current-system/sw/bin/zsh"];
  };

  nix = {
    useSandbox = true;
    readOnlyStore = true;
    buildCores = 4;

    binaryCachePublicKeys = [
       "hydra.cryp.to-1:8g6Hxvnp/O//5Q1bjjMTd5RO8ztTsG8DKPOAg9ANr2g=" # crypt.to
       "hydra.snabb.co-1:zPzKSJ1mynGtYEVbUR0QVZf9TLcaygz/OyzHlWo5AMM=" # snabb.co
       # "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" # reflex-frp
    ];
    trustedBinaryCaches = [
      "https://cache.nixos.org" "https://hydra.snabb.co" "http://hydra.cryp.to/"
      # "https://nixcache.reflex-frp.org"
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

  time.timeZone = "Europe/Berlin";

  nixpkgs.config.allowUnfree = true;

  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "de";
    defaultLocale = "de_DE.UTF-8";
  };

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      dejavu_fonts
      corefonts
      inconsolata
    ];
  };

  users = {
    mutableUsers = true;
    extraUsers.mhuber = {
      isNormalUser = true;
      group = "mhuber";
      uid = 1000;
      extraGroups = [ "wheel" "audio" "video" "vboxusers" "docker" "networkmanager" "dialout" ];
      createHome = true;
      home = "/home/mhuber";
      shell = "/run/current-system/sw/bin/zsh";
      password = "dummy";
    };
    extraGroups = {
      mhuber = {
        gid = 1000;
      };
    };
  };

  system = {
    activationScripts.media = ''
      mkdir -m 0755 -p /media /share
    '';
    autoUpgrade = {
      enable = true;
      channel = "https://nixos.org/channels/nixos-unstable";
    };
  };

  services = {
    # openssh.enable = true;
    # emacs = {
    #   enable = true;
    #   # package = import /home/mhuber/.emacs.d { pkgs = pkgs; };
    # };

    ntp.enable = true;

    nixosManual.showManual = true;
    acpid.enable = true;
    clamav = {
      daemon.enable = true;
      updater.enable = true;
    };
    vsftpd = {
      enable = false;
      userlist = ["mhuber"];
      localUsers = true;
      extraConfig = ''
        listen_port=9136
      '';
    };
  };

  security.setuidPrograms = [ "slock" "pmount" "pumount" ];

  programs = {
    zsh.enable = true;
    ssh.startAgent = false;
  };
}
