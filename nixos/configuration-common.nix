{ config, pkgs, lib, ... }:

let
  hasVBox = config.virtualisation.virtualbox.host.enable;
  hasDocker = config.virtualisation.docker.enable;
  hasnm = config.networking.networkmanager.enable;

in {
  imports = [
    ./profiles/oh-my-zsh.nix
    ./profiles/terminal.nix
  ];

  boot = {
    # kernelPackages = pkgs.linuxPackages_testing;
    kernelModules = [ "fuse" "kvm-intel" "coretemp" ];
    cleanTmpDir = true;
  };

  networking = {
    networkmanager.enable = true;
    firewall = {
      enable = true;
      # allowedTCPPorts = [ 80 443 ];
      allowPing = false;
    };
  };

  environment.systemPackages = with pkgs; [
    kbd
    wget curl
    git git-lfs
    ranger
    pmount fuse usbutils
    acpi acpid
    cryptsetup
    rsnapshot
    stow
  ];

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

    # nixPath = [ "/etc/nixos/path" "nixos-config=/etc/nixos/configuration.nix" ];
    # nixPath = [ "nixpkgs=http://nixos.org/channels/nixos-16.09/nixexprs.tar.xz" ];
  };

  time.timeZone = "Europe/Berlin";

  nixpkgs.config = {
    allowUnfree = true;
    # packageOverrides = super: let self = super.pkgs; in {
    # };
  };

  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "neo";
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
    mutableUsers = true; # one needs to change the password?
    extraUsers.mhuber = {
      isNormalUser = true;
      group = "mhuber";
      uid = 1000;
      extraGroups = [
        "wheel"
        "audio" "video"
        "dialout"
        "input" ]
        ++ pkgs.lib.optional hasnm "networkmanager"
        ++ pkgs.lib.optional hasVBox "vboxusers"
        ++ pkgs.lib.optional hasDocker "docker";
      home = "/home/mhuber";
      createHome = true;
      shell = "/run/current-system/sw/bin/zsh";
      # password = "dummy";
      initialPassword = lib.mkForce "dummy";
    };
    extraGroups.mhuber.gid = 1000;
  };

  system = {
    activationScripts.media = ''
      mkdir -m 0755 -p /media /share
    '';
    # autoUpgrade = {
    #   enable = true;
    #   channel = "https://nixos.org/channels/nixos-unstable";
    # };
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

  security.setuidPrograms = [ "pmount" "pumount" ];

  programs.ssh.startAgent = false;
}
