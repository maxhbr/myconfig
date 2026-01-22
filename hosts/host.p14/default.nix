# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
{
  imports = [
    ./ai.p14.nix
    ./hardware-configuration.nix
    inputs.nixos-hardware.nixosModules.common-cpu-intel-cpu-only
    # inputs.nixos-hardware.nixosModules.common-gpu-intel
    {
      # https://nixos.wiki/wiki/Intel_Graphics
      #  getting the device ID with: $ nix-shell -p pciutils --run "lspci -nn | grep VGA"
      boot.kernelParams = [ "i915.force_probe=46a6" ];
    }
    inputs.nixos-hardware.nixosModules.common-pc-laptop
    # inputs.nixos-hardware.nixosModules.common-pc-laptop-acpi_call
    inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad
    ../../hardware/efi.nix
    ../../hardware/notebook-generic.nix
    {
      myconfig.ai = {
        enable = true;
        coding.enable = false;
      };
    }
    {
      services.eternal-terminal = {
        enable = true;
        port = 22022;
      };
      networking.firewall.allowedTCPPorts = [ 22022 ];
      networking.firewall.allowedUDPPorts = [ 22022 ];
      home-manager.sharedModules =
        let
          p14-tmux-session = "p14";
          p14-tmux-session-script = pkgs.writeShellScriptBin "p14-tmux-session" ''
            # if session is not yet created, create it
            if ! tmux has-session -t ${p14-tmux-session}; then
              tmux new-session -d -s ${p14-tmux-session}
              tmux send-keys -t ${p14-tmux-session}:1 "btop" C-m
              tmux split-window -h -t ${p14-tmux-session}
              tmux send-keys -t ${p14-tmux-session}:1 "journalctl -f" C-m
              tmux split-window -v -t ${p14-tmux-session}
            fi
            exec tmux attach-session -t ${p14-tmux-session}
          '';
        in
        [ { home.packages = [ p14-tmux-session-script ]; } ];
    }
    # {
    #   services.openssh = {
    #     listenAddresses = [
    #       {
    #         addr = (myconfig.metadatalib.getWgIp "${config.networking.hostName}");
    #         port = 22;
    #       }
    #       {
    #         addr = (myconfig.metadatalib.getIp "${config.networking.hostName}");
    #         port = 22;
    #       }
    #       {
    #         addr = "127.0.0.1";
    #         port = 22;
    #       }
    #     ];
    #   };
    # }
    {
      config =
        let
          tmux-session = "btops";
          tmux-btops = pkgs.writeShellScriptBin "tmux-btops" ''
            # if session is not yet created, create it
            if ! tmux has-session -t ${tmux-session}; then
              tmux new-session -d -s ${tmux-session}
              tmux send-keys -t ${tmux-session}:1 "btop" C-m
              tmux split-window -h -t ${tmux-session}
              tmux send-keys -t ${tmux-session}:1 "et -x  mhuber@spare.wg0:22022 --command btop" C-m
              tmux split-window -v -t ${tmux-session}
              tmux send-keys -t ${tmux-session}:1 "et -x  mhuber@workstation.wg0:22022 --command btop" C-m
              tmux split-window -v -t ${tmux-session}
            fi
            exec tmux attach-session -t ${tmux-session}
          '';
        in
        {
          home-manager.sharedModules = [ { home.packages = with pkgs; [ tmux-btops ]; } ];
        };
    }
    # {
    #   fileSystems."/home/mhuber/MINE/Bilder/imgwork" = {
    #     device = "192.168.1.40:/imgwork";
    #     fsType = "nfs";
    #     options = [ "nofail" "soft" ];
    #   };
    # }
    (
      { pkgs, ... }:
      let
        fix-my-notebook = pkgs.writeShellScriptBin "fix-my-notebook" ''
          set -euo pipefail
          set -x
          systemctl restart bluetooth.service
          dbus-wm-environment wlroots
        '';
      in
      {
        config = {
          home-manager.sharedModules = [ { home.packages = with pkgs; [ fix-my-notebook ]; } ];
        };
      }
    )
    { environment.systemPackages = with pkgs; [ linuxPackages.usbip ]; }
    {
      programs.kdeconnect.enable = true;
    }
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
        messengers.enable = false;
        imagework.enable = true;
        imagework.myphoto.enable = true;
        obs.enable = true;
      };
      virtualisation.enable = true;
      dev = {
        embedded.enable = true;
      };
    };
    virtualisation = {
      # docker.enable = true;
      podman.enable = true;
      oci-containers = {
        backend = "podman";
      };
      # virtualbox.host.enable = true;
      # lxc.enable = true;
      # libvirtd.enable = true;
    };

    services.xserver.wacom.enable = false;
    services.xserver.digimend.enable = false;

    programs.gnupg.agent.enable = true;

    services.hardware.bolt.enable = true;

    services.gnome.gnome-keyring.enable = true;

    home-manager.sharedModules = [
      {
        programs.zsh.shellAliases = {
          upg-get-hostId = ''
            cksum /etc/machine-id | while read c rest; do printf "%x" $c; done
          '';
        };
      }
      {
        services.mako = {
          settings = {
            output = "eDP-1";
            default-timeout = 20000;
          };
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
    ];

    boot = {
      loader = {
        systemd-boot.enable = true;
        efi.canTouchEfiVariables = true;
      };

      binfmt.emulatedSystems = [
        "aarch64-linux"
        "armv6l-linux"
      ];
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
