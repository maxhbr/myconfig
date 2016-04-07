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
    pmount fuse
    acpi acpid
    gnumake cmake automake
    # grml-zsh-config

# for development
    meld
    leiningen clojure
    stack cabal-install cabal2nix

# Virtualization
    vagrant
    docker

# For email setup
    mutt-with-sidebar
    offlineimap msmtp gnupg abook notmuch

# git
    gitAndTools.gitFull
    # git
    # gitMinimal
    # gitAndTools.git-annex

# encryption
    cryptsetup

# password store
    pass

# tex
    (pkgs.texLiveAggregationFun { paths = [ pkgs.texLive pkgs.texLiveExtra pkgs.texLiveBeamer pkgs.texLiveCMSuper]; })

# for the desktop environmen
    xlibs.xmodmap xlibs.xset xlibs.setxkbmap
    xorg.xbacklight
    slock
    arandr
    dmenu
    scrot
    unclutter
    feh
    redshift
    rxvt_unicode_with-plugins rxvt_unicode.terminfo
    roxterm
    chromium luakit
    trayer networkmanagerapplet
    mupdf zathura llpp
    ] ++ hsPackages;
###############################################################################
in {
  imports =
    [
      ./hardware-configuration.nix
    ];

  hardware = {
    bluetooth.enable = false;
    opengl.driSupport32Bit = true;
  };

  boot = {
    # kernelPackages = pkgs.linuxPackages_testing;
    kernelPackages = pkgs.linuxPackages_4_3;
    kernelModules = [ "fuse" "kvm-intel" "coretemp" ];
    cleanTmpDir = true;
 #  loader.grub = {
 #    enable = true;
 #    version = 2;
 #    device = "/dev/sda";
 #    memtest86.enable = true;
 #  };
    loader.gummiboot.enable = true;
    initrd = {
        supportedFilesystems = [ "luks" ];
        luks.devices = [ { device = "/dev/sda2"; name = "crypted"; } ];
    };
  };

  nix = {
    useChroot = true;
    readOnlyStore = true;
    buildCores = 4;
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

  # nixpkgs.config.allowUnfree = true;

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
    # openssh.enable = true;
    ntp.enable = true;
    printing.enable = true;
    logind.extraConfig = "HandleLidSwitch=ignore";
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

      desktopManager = {
        xterm.enable = false;
        default = "none";
      };
      displayManager = {
        slim = {
          enable = true;
          defaultUser = "mhuber";
        };
        sessionCommands = ''
          ${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name left_ptr
          ${pkgs.xlibs.setxkbmap}/bin/setxkbmap de neo
          ${pkgs.xorg.xrdb}/bin/xrdb --merge ~/.Xresources
          if test -e $HOME/.background-image; then
            ${pkgs.feh}/bin/feh --bg-center $HOME/.background-image
          fi
          ${pkgs.redshift}/bin/redshift -l 48.2:10.8 &
          ${pkgs.rxvt_unicode_with-plugins}/bin/urxvtd -q -f -o &
        '';
      };

      startGnuPGAgent = true;

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
#     tlp = {
#       enable = true;
#       extraConfig = ''
# MAX_LOST_WORK_SECS_ON_BAT=15

# # Battery charge thresholds (ThinkPad only, tp-smapi or acpi-call kernel module
# # required). Charging starts when the remaining capacity falls below the
# # START_CHARGE_TRESH value and stops when exceeding the STOP_CHARGE_TRESH value.
# # Main / Internal battery (values in %)
# START_CHARGE_THRESH_BAT0=75
# STOP_CHARGE_THRESH_BAT0=90
# # Ultrabay / Slice / Replaceable battery (values in %)
# START_CHARGE_THRESH_BAT1=75
# STOP_CHARGE_THRESH_BAT1=90
#       '';
#     };
  };

  users = {
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
      vboxusers.members = [ "mhuber" ];
      docker.members = [ "mhuber" ];
    };
  };

  virtualisation = {
    docker.enable = true;
    virtualbox.host.enable = true;
  };

  programs = {
    zsh.enable = true;
    ssh.startAgent = false;
  };

  system = {
    activationScripts.media = ''
      mkdir -m 0755 -p /media /share
    '';
    autoUpgrade = {
      enable = true;
      channel = https://nixos.org/channels/nixos-15.09;
    };
  };

  # systemd.user.services = {
  #   emacs = {
  #     description = "Emacs: the extensible, self-documenting text editor";
  #     serviceConfig = {
  #       Type      = "forking";
  #       ExecStart = "${pkgs.emacs}/bin/emacs --daemon --user=mhuber";
  #       ExecStop  = "${pkgs.emacs}/bin/emacsclient --eval \"(kill-emacs)\"";
  #       Restart   = "always";
  #     };
  #     wantedBy = [ "default.target" ];
  #     environment = {
  #       SSH_AUTH_SOCK = "%t/keyring/ssh";
  #       # Make sure aspell will find its dictionaries
  #       ASPELL_CONF   = "dict-dir /run/current-system/sw/lib/aspell";
  #       # Make sure locate will find its database
  #       LOCATE_PATH   = "/var/cache/locatedb";
  #     };
  #     enable = true;
  #   };
  #   offlineimap = {
  #     description = "Start offlineimap as a daemon";
  #     serviceConfig = {
  #       Type       = "forking";
  #       ExecStart  = "${pkgs.offlineimap}/bin/offlineimap";
  #       KillSignal = "SIGUSR2";
  #       Restart    = "always";
  #     };
  #     wantedBy = [ "multi-user.target" ];
  #     wants = [ "network-online.target" ];
  #     after = [ "network.target" ];
  #     enable = true;
  #   };
  # };
}

# vim:set ts=2 sw=2 sts=2 et foldmethod=marker foldlevel=0 foldmarker={{{,}}}:
