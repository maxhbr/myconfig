{ config, lib, pkgs, ... }: {
  imports = [ ./hardware-configuration.nix ];

  config = {
    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = lib.mkForce "23.11"; # Did you read the comment?

    # Use the systemd-boot EFI boot loader.
    boot.loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      grub.enable = false;
    };

    myconfig = {
      desktop = {
        enable = true;
        wayland = {
          enable = true;
          desktop = "dwl";
          dwl.enable = true;
        };
      };
      desktop.full = false;
      headless.enable = true;
    };
    virtualisation.docker.enable = true;
    virtualisation.podman.enable = true;

    networking.hostName = "r6c";
    networking.hostId = "ac8aad7a";

    networking.networkmanager.enable =
      true; # Easiest to use and most distros use this by default.

    services.vsftpd.enable = lib.mkForce false; # fails to build on arm

    swapDevices = [{
      device = "/swapfile";
      priority = 0;
      size = 4096;
    }];

    # https://github.com/NixOS/nixpkgs/issues/154163
    # https://github.com/NixOS/nixpkgs/issues/111683#issuecomment-968435872
    # https://github.com/NixOS/nixpkgs/issues/126755#issuecomment-869149243
    nixpkgs.overlays = [
      (final: super: {
        makeModulesClosure = x:
          super.makeModulesClosure (x // { allowMissing = true; });
      })
    ];
  };
}
