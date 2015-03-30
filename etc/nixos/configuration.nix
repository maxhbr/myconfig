{ config, pkgs, ... }:

let
  hsPackages = with pkgs.haskellPackages; [
    xmonad xmobar
    ghc hlint pandoc pointfree pointful
  ];
in {
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
  };

  networking = {
    hostName = "nixos"; # Define your hostname.
    hostId = "54510fe1";
    # wireless.enable = true;  # Enables wireless.
  };

  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "de";
    defaultLocale = "de_DE.UTF-8";
  };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment = {
    systemPackages = with pkgs; [
      wget htop vim zsh tmux
      gitAndTools.gitFull
      xlibs.xmodmap xlibs.xset
      dmenu scrot unclutter feh redshift
      rxvt roxterm chromium
    ] ++ hsPackages;
  };

  fonts.fonts = with pkgs; [
    dejavu_fonts
    corefonts
    inconsolata
  ];

  powerManagement.enable = true;

  time.timeZone = "Europe/Berlin";

  services = {
    openssh.enable = true;
    printing.enable = true;
    xserver = {
      enable = true;
      layout = "de"; # TODO: neo
      # xkbOptions = "eurosign:e";
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
        ${pkgs.redshift}/bin/redshift -l 48.2:10.8 &
        ${pkgs.roxterm}/bin/roxterm &
      '';

      # synaptics.additionalOptions = ''
      #   Option "VertScrollDelta" "-100"
      #   Option "HorizScrollDelta" "-100"
      #   '';
      # synaptics.buttonsMap = [ 1 3 2 ];
      # synaptics.enable = true;
      # synaptics.tapButtons = false;
      # synaptics.fingersMap = [ 0 0 0 ];
      # synaptics.twoFingerScroll = true;
      # synaptics.vertEdgeScroll = false;
    };
  };

  users.extraUsers.mhuber = {
    isNormalUser = true;
    group = "users";
    uid = 1000;
    extraGroups = [ "wheel" ];
    createHome = true;
    home = "/home/mhuber";
    shell = "/run/current-system/sw/bin/zsh";
  };

}

# vim:set ts=2 sw=2 sts=2 et foldmethod=marker foldlevel=0 foldmarker={{{,}}}:
