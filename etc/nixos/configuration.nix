{ config, pkgs, ... }:

let
  hsPackages = with pkgs.haskellPackages; [
    xmonad xmobar yeganesh
    cabal-install
    ghc hlint pandoc pointfree pointful hdevtools
  ];
  myPackages = with pkgs; [
    kbd
    wget curl elinks w3m
    htop powertop
    emacs
    vim
    tmux
    ranger

# for development
    leiningen clojure
    cabal2nix

# Virtualization
    # virtualbox
    docker

# For email setup
    mutt-with-sidebar
    offlineimap
    msmtp
    gnupg
    abook

# git
    # gitAndTools.gitFull
    # git
    gitMinimal
    # gitAndTools.git-annex

# for the desktop environmen
    xlibs.xmodmap xlibs.xset xlibs.setxkbmap
    dmenu
    scrot
    unclutter
    feh
    redshift
    rxvt_unicode_with-plugins
    roxterm
    chromium
    trayer networkmanagerapplet
    ] ++ hsPackages;
###############################################################################
in {
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot = {
    # kernelPackages = pkgs.linuxPackages_4_0;
    kernelPackages = pkgs.linuxPackages_testing;
    kernelModules = [ "fuse" "kvm-intel" "coretemp" ];
    cleanTmpDir = true;
 #  loader.grub = {
 #    enable = true;
 #    version = 2;
 #    device = "/dev/sda";
 #    memtest86.enable = true;
 #  };
    loader.gummiboot.enable = true;
  };

  nix = {
    useChroot = true;
    readOnlyStore = true;
    buildCores = 4;
    binaryCaches = [
      "http://cache.nixos.org/"
      "http://hydra.nixos.org/"
      "http://hydra.cryp.to/"
    ];
    trustedBinaryCaches = [
      "http://hydra.cryp.to/"
    ];
    extraOptions = ''
      gc-keep-outputs = true
      gc-keep-derivations = true
      auto-optimise-store = true
      binary-caches-parallel-connections = 10
    '';
  };

  nixpkgs.config.allowUnfree = true;

  networking = {
    networkmanager.enable = true;
    # wireless.enable = true;
    hostName = "nixos";
    hostId = "54510fe1"; # head -c4 /dev/urandom | od -A none -t x4
    firewall = {
      enable = true;
      # allowedTCPPorts = [ 80 443 ];
      allowPing = false;
    };
  };

  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "de";
    defaultLocale = "de_DE.UTF-8";
  };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment = {
    systemPackages = myPackages;
    # shellAliases = {
    #   ll = "ls -l";
    # };
    shells = ["/run/current-system/sw/bin/zsh"];
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

  powerManagement.enable = true;

  time.timeZone = "Europe/Berlin";

  services = {
    openssh.enable = true;
    ntp.enable = true;
    printing.enable = true;
    xserver = {
      enable = true;
      autorun = true;
      layout = "de"; # TODO: neo
      xkbOptions = "neo";
      windowManager = {
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
        };
        default = "xmonad";
      };
      desktopManager.default = "none";
      displayManager.sessionCommands = ''
        ${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name left_ptr &
        ${pkgs.xlibs.setxkbmap}/bin/setxkbmap de neo
        ${pkgs.redshift}/bin/redshift -l 48.2:10.8 &
        ${pkgs.rxvt_unicode_with-plugins}/bin/urxvtd -q -f -o &
      '';

      # startGnuPGAgent = true;

      synaptics.additionalOptions = ''
        Option "VertScrollDelta" "-100"
        Option "HorizScrollDelta" "-100"
      '';
      synaptics.buttonsMap = [ 1 3 2 ];
      synaptics.enable = true;
      synaptics.tapButtons = false;
      synaptics.fingersMap = [ 0 0 0 ];
      synaptics.twoFingerScroll = true;
      synaptics.vertEdgeScroll = false;
    };

    nixosManual.showManual = true;
    acpid.enable = true;


    # TODO: emacs service?
    # see: https://github.com/ardumont/dot-files/blob/master/nixos/services.nix#L28
  };

  users = {
    extraUsers.mhuber = {
      isNormalUser = true;
      group = "users";
      uid = 1000;
      extraGroups = [ "wheel" "audio" "video" "vboxusers" "docker" "networkmanager" "dialout" ];
      createHome = true;
      home = "/home/mhuber";
      shell = "/run/current-system/sw/bin/zsh";
      password = "dummy";
    };
    extraGroups.docker.members = [ "mhuber" ];
  };
  virtualisation.docker.enable = true;
  # services.virtualboxHost.enable = false;
  programs.zsh.enable = true;


  system.activationScripts.media = ''
    mkdir -m 0755 -p /media /share
  '';

systemd.user.services = {
    emacs = {
      description = "Emacs: the extensible, self-documenting text editor";
      serviceConfig = {
        Type      = "forking";
        ExecStart = "${pkgs.emacs}/bin/emacs --daemon";
        ExecStop  = "${pkgs.emacs}/bin/emacsclient --eval \"(kill-emacs)\"";
        Restart   = "always";
      };
      wantedBy = [ "default.target" ];
      environment = {
        # Make sure aspell will find its dictionaries
        ASPELL_CONF     = "dict-dir /run/current-system/sw/lib/aspell";
        # Make sure locate will find its database
        LOCATE_PATH     = "/var/cache/locatedb";
      };
      enable = true;
    };
  };
}

# vim:set ts=2 sw=2 sts=2 et foldmethod=marker foldlevel=0 foldmarker={{{,}}}:
