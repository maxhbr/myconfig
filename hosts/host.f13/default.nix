# Copyright 2016-2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, myconfig, inputs, ... }: {
  imports = [
    ./hardware-configuration.nix
    ./impermanence.nix
    inputs.nixos-hardware.nixosModules.common-cpu-intel-cpu-only
    inputs.nixos-hardware.nixosModules.common-pc-laptop
    inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
    inputs.nixos-hardware.nixosModules.framework-amd-ai-300-series
    ../../hardware/efi.nix
    ../../hardware/notebook-generic.nix
    ./mykeylight
    # ./role.work
    # ./ai.f13.nix
    {
      services.openssh = {
        listenAddresses = [{
          addr = "127.0.0.1";
          port = 22;
        }];
      };
    }
    { environment.systemPackages = with pkgs; [ linuxPackages.usbip ]; }
    {
      programs.kdeconnect.enable = true;
    }
  ];

  config = {
    networking.hostName = "f13";
    networking.hostId = "00000f13";
    networking.useDHCP = false;
    networking.interfaces.wlp192s0.useDHCP = true;
    myconfig = {
      desktop = {
        enable = true;
        wayland = {
          enable = true;
          selectedSessions = [
            "niri"
            # "niri-plain"
            # "labwc"
            # "river"
            # # "plasma6"
            # # "dwl"
            # # "qtile"
            # # "hyprland"
          ];
          niri.additionalConfigKdl = ''
            // You can configure outputs by their name, which you can find
            // by running `niri msg outputs` while inside a niri instance.
            // The built-in laptop monitor is usually called "eDP-1".
            // Find more information on the wiki:
            // https://github.com/YaLTeR/niri/wiki/Configuration:-Outputs
            // Remember to uncomment the node by removing "/-"!
            output "eDP-1" {
                // Uncomment this line to disable this output.
                // off

                // Resolution and, optionally, refresh rate of the output.
                // The format is "<width>x<height>" or "<width>x<height>@<refresh rate>".
                // If the refresh rate is omitted, niri will pick the highest refresh rate
                // for the resolution.
                // If the mode is omitted altogether or is invalid, niri will pick one automatically.
                // Run `niri msg outputs` while inside a niri instance to list all outputs and their modes.
                mode "2880x1920@120.000"

                // Scale is a floating-point number, but at the moment only integer values work.
                scale 1.30

                // Transform allows to rotate the output counter-clockwise, valid values are:
                // normal, 90, 180, 270, flipped, flipped-90, flipped-180 and flipped-270.
                transform "normal"

                // Position of the output in the global coordinate space.
                // This affects directional monitor actions like "focus-monitor-left", and cursor movement.
                // The cursor can only move between directly adjacent outputs.
                // Output scale and rotation has to be taken into account for positioning:
                // outputs are sized in logical, or scaled, pixels.
                // For example, a 3840×2160 output with scale 2.0 will have a logical size of 1920×1080,
                // so to put another output directly adjacent to it on the right, set its x to 1920.
                // If the position is unset or results in an overlap, the output is instead placed
                // automatically.
                position x=0 y=0
            }
          '';
        };
        # messengers.enable = true;
        # myphoto.enable = true;
        # obs.enable = true;
        # # cad.enable = true;
        # deskreen.enable = true;
      };
      # email.enable = true;
      # virtualisation.enable = true;
      # editor.emacs.enable = false;
      # dev = {
      #   compliance.enable = true;
      #   go.enable = false;
      #   haskell.enable = true;
      #   network.enable = true;
      #   nodejs.enable = true;
      #   # ruby.enable = true;
      #   python.enable = true;
      #   # rust.enable = true;
      #   # elixir.enable = false;
      #   # zephyr.enable = true;
      # };
    };
    # virtualisation = {
    #   docker.enable = true;
    #   podman.enable = true;
    #   oci-containers = { backend = "podman"; };
    #   # virtualbox.host.enable = true;
    #   # lxc.enable = true;
    #   libvirtd.enable = true;
    # };

    # services.xserver.wacom.enable = false;
    # services.xserver.digimend.enable = false;

    programs.gnupg.agent.enable = true;

    services.hardware.bolt.enable = true;

    services.logind.extraConfig = ''
      HandlePowerKey=suspend
      RuntimeDirectorySize=8G
    '';
    services.gnome.gnome-keyring.enable = true;

    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          joplin-desktop
        ];
      }
      {
        services.mako = {
          settings = {
            output = "eDP-1";
            default-timeout = 20000;
          };
        };
      }
      {
        home.packages = with pkgs.helper; [
          (connectBtDevice {
            name = "WF-1000XM5";
            id = "AC:80:0A:2A:10:6F";
          })
          (connectBtDevice {
            name = "Px7";
            id = "EC:66:D1:B4:C8:3B";
          })
          (connectBtDevice {
            name = "Px7s2e";
            id = "EC:66:D1:BD:E4:98";
          })
          (connectBtDevice {
            name = "Streambot";
            id = "E9:08:EF:60:57:21";
          })
        ];
      }
    ];

    boot = {
      loader = {
        systemd-boot.enable = true;
        efi.canTouchEfiVariables = true;
      };

      binfmt.emulatedSystems = [ "aarch64-linux" "armv6l-linux" ];
      kernelPackages = lib.mkForce pkgs.linuxPackages_latest;
      initrd = {
        supportedFilesystems = [ "nfs" ];
        kernelModules = [ "nfs" ];
      };
    };

    hardware.enableRedistributableFirmware = true;

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = lib.mkForce "25.05"; # Did you read the comment?
  };
}
