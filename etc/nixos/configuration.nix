# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  hsPackages = with pkgs.haskellPackages; [
    xmonad
    xmobar
    ghc
    hlint
    pandoc
    pointfree
    pointful
  ];
in {
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
  };
  # Define on which hard drive you want to install Grub.

  networking = {
    hostName = "nixos"; # Define your hostname.
    hostId = "54510fe1";
    # wireless.enable = true;  # Enables wireless.
  };

  # Select internationalisation properties.
  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "de";
    defaultLocale = "de_DE.UTF-8";
  };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment = {
    systemPackages = with pkgs; [
      wget pkgs.htop
      vim zsh tmux
      gitAndTools.gitFull
      xlibs.xmodmap
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
      layout = "de";
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
        ${pkgs.redshift}/bin/redshift -l 48.2:10.8 &
        ${pkgs.roxterm}/bin/roxterm &
      '';
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
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
