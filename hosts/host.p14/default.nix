# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, myconfig, ... }: {
  imports = [
    ./hardware-configuration.nix
    ../../hardware/efi.nix
    ../../hardware/notebook-generic.nix
    # ./hardware.TP-Link_Archer_TX20U.nix
    {
      boot.loader.systemd-boot.enable = true;
      boot.loader.efi.canTouchEfiVariables = true;

      networking.useDHCP = false;
      networking.interfaces.wlp0s20f3.useDHCP = true;
      # networking.interfaces.enp82s0u2u1u2.useDHCP = true;
    }
    ../../hardware/footswitch.nix
    ../../hardware/blink1.nix
    ../../hardware/unifying.nix
    ./hardware.hantek
    ./mykeylight
    ./role.work
    ./programs.khal.nix
    # ./a7iiiAsWebcam.nix
    {
      services.openssh = {
        listenAddresses = [{
          addr = "127.0.0.1";
          port = 22;
        }];
      };
    }
    {
      boot.kernelPackages = lib.mkForce pkgs.linuxPackages_latest;
    }
    # fun
    ../host.workstation/gaming/games.steam
    {
      home-manager.sharedModules =
        [{ home.packages = with pkgs; [ mindustry-wayland ]; }];
    }
    # {
    #   home-manager.sharedModules =
    #     [{ home.packages = with pkgs; [ libreoffice ]; }];
    # }
  ];

  config = {
    networking.hostName = "p14";
    networking.hostId = "1ea9689e";
    myconfig = {
      desktop = {
        enable = true;
        wayland = {
          enable = true;
          desktop = "dwl";
          dwl.enable = true;
          # river.enable = true;
          labwc.enable = true;
          # gnome.enable = true; # konflicts with sway

          #disabled:
          # sway.enable = true;
          #experimental:
          # wayfire.enable = true;
          # qtile.enable = true;
          # hyprland.enable = true;
          # vivarium.enable = true;
          #bloated:
          # kde.enable = true;
          #dead or buggy:
          # hikari.enable = true;
          # newm.enable = true;
        };
        messengers.enable = true;
        imagework.enable = true;
        obs.enable = true;
        cad.enable = true;
        deskreen.enable = true;
      };
      email.enable = true;
      virtualisation.enable = true;
      dev = {
        compliance.enable = true;
        go.enable = false;
        haskell.enable = true;
        network.enable = true;
        nodejs.enable = true;
        # ruby.enable = true;
        # rust.enable = true;
        # elixir.enable = false;
        embedded.enable = true;
      };
    };
    virtualisation.docker.enable = true;
    virtualisation.podman.enable = true;
    # virtualisation.libvirtd.enable = true;
    virtualisation.virtualbox.host.enable = false;

    services.xserver.wacom.enable = true;
    # services.xserver.digimend.enable = true;

    services.hardware.bolt.enable = true;

    services.logind.extraConfig = ''
      HandlePowerKey=suspend
      RuntimeDirectorySize=8G
    '';
    services.gnome.gnome-keyring.enable = true;

    home-manager.sharedModules = [
      {
        home.packages = with pkgs;
          [
            # rdesktop
            google-chrome # for netflix and stadia
          ];
        programs.zsh.shellAliases = {
          upg-get-hostId = ''
            cksum /etc/machine-id | while read c rest; do printf "%x" $c; done
          '';
        };
      }
      {
        services.mako = {
          output = "eDP-1";
          defaultTimeout = lib.mkForce 20000;
        };
        services.kanshi = {
          enable = lib.mkForce true;
          profiles = {
            # get list via "swaymsg -t get_outputs"
            undocked = {
              outputs = [{
                criteria = "eDP-1";
                mode = "1920x1200@60Hz";
                position = "0,0";
                scale = 1.0;
              }];
              exec = [ "${pkgs.mykeylight}/bin/mykeylight-off" ];
            };
            dp4 = {
              outputs = [
                {
                  criteria = "eDP-1";
                  mode = "1920x1200@60Hz";
                  position = "0,0";
                  scale = 1.0;
                }
                {
                  criteria = "DP-4";
                  mode = "2560x1440@59.951Hz";
                  position = "1920,0";
                  scale = 1.0;
                }
              ];
              exec = [ "${pkgs.mykeylight}/bin/mykeylight-off" ];
            };
            docked = {
              outputs = [
                {
                  criteria = "eDP-1";
                  mode = "1920x1200@60Hz";
                  position = "0,0";
                  scale = 1.0;
                }
                {
                  criteria = "DP-5";
                  mode = "2560x1440@59.951Hz";
                  position = "1920,0";
                  scale = 1.0;
                }
                {
                  criteria = "DP-7";
                  mode = "1920x1080@60Hz";
                  position = "2240,1440";
                  scale = 1.0;
                }
              ];
              exec = [ "${pkgs.mykeylight}/bin/mykeylight-off" ];
            };
            # docked2 = {
            #   outputs = [
            #     {
            #       criteria = "eDP-1";
            #       mode = "1920x1200@60Hz";
            #       position = "0,0";
            #       scale = 1.0;
            #     }
            #     {
            #       criteria = "DP-5";
            #       mode = "2560x1440@59.951Hz";
            #       position = "1920,0";
            #       scale = 1.0;
            #     }
            #     {
            #       criteria = "DP-4";
            #       mode = "1920x1080@60Hz";
            #       position = "2240,1440";
            #       scale = 1.0;
            #     }
            #   ];
            #   exec = [ "${pkgs.mykeylight}/bin/mykeylight-off" ];
            # };
            # docked3 = {
            #   outputs = [
            #     {
            #       criteria = "eDP-1";
            #       mode = "1920x1200@60Hz";
            #       position = "0,0";
            #       scale = 1.0;
            #     }
            #     {
            #       criteria = "DP-6";
            #       mode = "2560x1440@59.951Hz";
            #       position = "1920,0";
            #       scale = 1.0;
            #     }
            #     {
            #       criteria = "DP-4";
            #       mode = "1920x1080@60Hz";
            #       position = "2240,1440";
            #       scale = 1.0;
            #     }
            #   ];
            #   exec = [ "${pkgs.mykeylight}/bin/mykeylight-off" ];
            # };
          };
        };
      }
    ];

    boot.binfmt.emulatedSystems = [ "aarch64-linux" "armv6l-linux" ];

    hardware.enableRedistributableFirmware = true;

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = lib.mkForce "23.05"; # Did you read the comment?
  };
}
