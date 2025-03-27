# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, myconfig, inputs, ... }: {
  imports = [
    ./hardware-configuration.nix
    inputs.nixos-hardware.nixosModules.common-cpu-intel-cpu-only
    # inputs.nixos-hardware.nixosModules.common-gpu-intel
    {
      # https://nixos.wiki/wiki/Intel_Graphics
      #  getting the device ID with: $ nix-shell -p pciutils --run "lspci -nn | grep VGA"
      boot.kernelParams = [ "i915.force_probe=46a6" ];

      # # https://nixos.wiki/wiki/Accelerated_Video_Playback
      #   nixpkgs.config.packageOverrides = pkgs: {
      #   intel-vaapi-driver = pkgs.intel-vaapi-driver.override { enableHybridCodec = true; };
      # };
      # hardware.opengl = {
      #   enable = true;
      #   extraPackages = with pkgs; [
      #     intel-media-driver # LIBVA_DRIVER_NAME=iHD
      #     intel-vaapi-driver # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      #     libvdpau-va-gl
      #   ];
      # };
      # environment.sessionVariables = { LIBVA_DRIVER_NAME = "iHD"; }; # Force intel-media-driver
    }
    inputs.nixos-hardware.nixosModules.common-pc-laptop
    # inputs.nixos-hardware.nixosModules.common-pc-laptop-acpi_call
    inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad
    ../../hardware/efi.nix
    ../../hardware/notebook-generic.nix
    ../../hardware/footswitch.nix
    ../../hardware/blink1.nix
    # ../../hardware/unifying.nix
    ./hardware.hantek
    ./mykeylight
    ./smarthome.nix
    ./role.work
    ./programs.khal.nix
    ./ai-tooling.nix
    {
      services.openssh = {
        listenAddresses = [{
          addr = "127.0.0.1";
          port = 22;
        }];
      };
    }
    # {
    #   fileSystems."/home/mhuber/MINE/Bilder/imgwork" = {
    #     device = "192.168.1.40:/imgwork";
    #     fsType = "nfs";
    #     options = [ "nofail" "soft" ];
    #   };
    # }
    # ../host.workstation/gaming/games.steam
    # ({config, pkgs, ...}: {
    #   config = lib.mkIf config.myconfig.desktop.wayland.hyprland.enable {
    #     home-manager.sharedModules = [({config, ...}: let
    #       hyprctl = "${config.wayland.windowManager.hyprland.package}/bin/hyprctl";
    #       eDP-1 = "eDP-1,1920x1200,0x0,1";
    #       DP-5 = "DP-5,2560x1440,1920x0,1"; # Dell Inc. DELL U2719D 7RVLSS2 (DP-5)
    #       DP-3 = "DP-3,1920x1080,2240x1440,1"; #  HAT Kamvas Pro 13  (DP-3 via HDMI)
    #     in {
    #       home.packages = with pkgs; [ 
    #         (writeShellScriptBin "hyprctl-monitors-home"  "${hyprctl} hyprctl --batch 'keyword monitor ${eDP-1}; keyword monitor ${DP-5}; keyword monitor ${DP-3}'")
    #       ];
    #       # wayland.windowManager.hyprland = {
    #       #   extraConfig = ''
    #       #     device:opentabletdriver-virtual-artist-tablet {
    #       #         output=DP-5
    #       #     }
    #       #   '';
    #       # };
    #     })];
    #   };
    # }
    # )
    ({ pkgs, ... }:
      let
        fix-my-notebook = pkgs.writeShellScriptBin "fix-my-notebook" ''
          set -euo pipefail
          set -x
          systemctl restart bluetooth.service
          dbus-wm-environment wlroots
        '';
      in {
        config = {
          home-manager.sharedModules =
            [{ home.packages = with pkgs; [ fix-my-notebook ]; }];
        };
      })
    { environment.systemPackages = with pkgs; [ linuxPackages.usbip ]; }
    {
      programs.kdeconnect.enable = true;
    }

    # Testing: Thunderbolt GPUs
    ./eGPU.p14.nix
  ];

  config = {
    networking.hostName = "p14";
    networking.hostId = "1ea9689e";
    networking.useDHCP = false;
    networking.interfaces.wlp0s20f3.useDHCP = true;
    # networking.interfaces.enp82s0u2u1u2.useDHCP = true;
    myconfig = {
      desktop = {
        enable = true;
        wayland = {
          enable = true;
          selectedSessions = [
            "niri"
            "niri-plain"
            "labwc"
            "river"
            "plasma6"
            # "dwl"
            # "qtile"
            # "hyprland"
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
                mode "1920x1200@60.000"

                // Scale is a floating-point number, but at the moment only integer values work.
                scale 1.0

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
        messengers.enable = true;
        myphoto.enable = true;
        obs.enable = true;
        cad.enable = true;
        # deskreen.enable = true;
      };
      email.enable = true;
      virtualisation.enable = true;
      editor.emacs.enable = true;
      dev = {
        compliance.enable = true;
        go.enable = false;
        haskell.enable = true;
        network.enable = true;
        nodejs.enable = true;
        # ruby.enable = true;
        python.enable = true;
        # rust.enable = true;
        # elixir.enable = false;
        # zephyr.enable = true;
      };
    };
    virtualisation = {
      docker.enable = true;
      podman.enable = true;
      oci-containers = { backend = "podman"; };
      # virtualbox.host.enable = true;
      # lxc.enable = true;
      libvirtd.enable = true;
    };

    services.xserver.wacom.enable = false;
    services.xserver.digimend.enable = false;

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
          google-chrome # for netflix
          joplin-desktop
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
        # services.kanshi = {
        #   enable = lib.mkForce true;
        #   settings = {
        #     # get list via "swaymsg -t get_outputs"
        #     undocked = {
        #       outputs = [{
        #         criteria = "eDP-1";
        #         mode = "1920x1200@60Hz";
        #         position = "0,0";
        #         scale = 1.0;
        #       }];
        #       exec = [ "${pkgs.mykeylight}/bin/mykeylight-off" ];
        #     };
        #   };
        # };
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
            name = "Px8";
            id = "EC:66:D1:C6:94:14";
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
    system.stateVersion = lib.mkForce "23.05"; # Did you read the comment?
  };
}
