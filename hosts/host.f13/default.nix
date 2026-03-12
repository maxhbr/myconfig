# Copyright 2016-2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
let
  user = myconfig.user;
in
{
  imports = [
    ./specialisation.nix
    ./hardware-configuration.nix
    inputs.nixos-hardware.nixosModules.common-pc-laptop
    inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
    ./framework-amd-ai-300-series.nix # inputs.nixos-hardware.nixosModules.framework-amd-ai-300-series
    ../../hardware/efi.nix
    ../../hardware/notebook-generic.nix
    ../../hardware/Radeon890M.nix
    ../../hardware/RZ717.nix
    {
      config = {
        # Disable NetworkManager-wait-online to speed up boot time
        systemd.services.NetworkManager-wait-online.enable = false;
      };
    }
    ./hardware.framework.do-not-wake-on-input.nix
    ./suspend-debug.nix
    ./hardware.hantek
    ./mykeylight
    ./role.work
    ./ai.f13.nix
    {
      services.openssh = {
        listenAddresses = [
          # {
          #   addr = (myconfig.metadatalib.getWgIp "${config.networking.hostName}");
          #   port = 22;
          # }
          # # {
          # #   addr = (myconfig.metadatalib.getIp "${config.networking.hostName}");
          # #   port = 22;
          # # }
          {
            addr = "127.0.0.1";
            port = 22;
          }
        ];
      };
    }
    {
      home-manager.sharedModules = [ { home.packages = with pkgs; [ gftp ]; } ];
    }
    (
      { pkgs, ... }:
      let
        fix-my-notebook = pkgs.writeShellScriptBin "fix-my-notebook" ''
          set -euo pipefail
          set -x
          systemctl restart home-manager-${user}.service
          systemctl restart bluetooth.service
          ${config.myconfig.desktop.audio.fix-audio.script}/bin/fix-audio --no-mixer
        '';
      in
      {
        config = {
          myconfig.desktop.audio.fix-audio.enable = true;
          myconfig.desktop.audio.fix-audio.bluez_devices = [
            "AC:80:0A:2A:10:6F"
            "EC:66:D1:BD:E4:98"
            "EC:66:D1:B4:C8:3B"
          ];
          myconfig.desktop.audio.fix-audio.preferred_sinks_patterns = [
            "alsa_output.usb-FiiO_DigiHug_USB_Audio-01.analog-stereo"
            "alsa_output.pci-0000_c1_00.1.hdmi-stereo-extra2"
            "alsa_output.pci-0000_c1_00.6.analog-stereo"
            "usb-FiiO_DigiHug_USB_Audio-01.analog-stereo"
            "hdmi-stereo"
            "alsa_output.pci-0000_c1_00.6.analog-stereo"
          ];
          myconfig.desktop.audio.fix-audio.preferred_sources_patterns = [
            "alsa_input.usb-BLUE_MICROPHONE_Blue_Snowball_797_2020_07_31_01554-00.mono-fallback"
            "alsa_input.usb-046d_HD_Pro_Webcam_C920_9C1E301F-02.analog-stereo"
            "usb-BLUE_MICROPHONE_Blue_Snowball_797_2020_07_31_01554-00.mono-fallback"
            "usb-046d_HD_Pro_Webcam_C920_9C1E301F-02.analog-stereo"
            "alsa_input.pci-0000_c1_00.6.analog-stereo"
          ];
          home-manager.sharedModules = [ { home.packages = with pkgs; [ fix-my-notebook ]; } ];
        };
      }
    )
    {
      services.eternal-terminal = {
        enable = false;
      };
    }
    { programs.kdeconnect.enable = true; }
    # NOTE: amd_pstate=active and power-profiles-daemon are already set by
    # framework-amd-ai-300-series.nix -> amd.nix -> pstate.nix
    {
      myconfig.desktop.wayland.waybar.doesFileExistChecks = [
        "/home/${user}/myconfig/.myconfig.ready"
      ];
    }
  ];

  config = {
    networking = {
      hostName = "f13";
      hostId = "00000${config.networking.hostName}";

      #       dhcpcd.extraConfig = ''
      # # interface eth0
      # #   metric 100

      # interface wlp192s0
      #   metric 200
      #       '';
    };
    myconfig = {
      persistence.impermanence = {
        enable = true;
        soft_permanence_for_boot = false;
        tmpfs_size = "30%";
        btrfs_device = "/dev/mapper/enc-pv";
        # sdd
        # ├─sdd1         ext4        1.0   backupUnenc 160b2393-6f8f-4a74-97b5-e30876a604d3
        # └─sdd2         crypto_LUKS 2                 eb6f6825-be7c-4666-8dfe-0d7ef592287c
        #   └─backupLuks btrfs             backupEnc   b3a6f87c-5ffb-4cfd-96f1-a7d6acd50d1a
        btrbk_priv = {
          device = "/dev/disk/by-uuid/b3a6f87c-5ffb-4cfd-96f1-a7d6acd50d1a";
          luks_device = "/dev/disk/by-uuid/eb6f6825-be7c-4666-8dfe-0d7ef592287c";
          luks_keyfile = "/home/mhuber/.password-store/hardware/backup/backupLuks_eb6f6825-be7c-4666-8dfe-0d7ef592287c.key";
        };
        btrbk_work = {
          device = "/dev/disk/by-uuid/8e3c7395-c663-4080-9463-3b8a18bd7ad3";
          luks_device = "/dev/disk/by-uuid/51df4369-0063-446a-96db-6d08b6349935";
          luks_keyfile = "/home/mhuber/.password-store/hardware/backup/backupLuks_aa3f2db7-7b1a-4232-9b42-15270e9e5a79.key";
        };
      };
      desktop = {
        enable = true;
        wayland = {
          enable = true;
          selectedSessions = [
            "niri"
            # "niri-plain"
            "labwc"
            # "river"
            # "plasma6"
            # "dwl"
            # "qtile"
            # "hyprland"
          ];
          niri.additionalConfigKdl = ''
            output "eDP-1" {
                mode "2880x1920@120.000"
                scale 1.30
                transform "normal"
                position x=0 y=0
            }
          '';
        };
        messengers.enable = true;
        obs.enable = true;
        imagework.enable = true; # https://github.com/NixOS/nixpkgs/issues/425306
        imagework.myphoto.enable = true;
        cad.enable = true;
        # joplin-desktop.enable = true;
        logseq.enable = true;
      };
      v4l2.enable = true;
      email = {
        # enable = true; # this is set in the priv repo
        clients = [
          "aerc"
          "neomutt"
          "thunderbird"
          "himalaya"
        ];
      };
      virtualisation.enable = true;
      dev = {
        # compliance.enable = true;
        # go.enable = false;
        haskell.enable = true;
        network.enable = true;
        # nodejs.enable = true;
        # ruby.enable = true;
        # python.enable = true;
        # rust.enable = true;
        # elixir.enable = false;
        # zephyr.enable = true;
        embedded.enable = true;
      };
    };
    virtualisation = {
      # docker.enable = true;
      podman.enable = true;
      # virtualbox.host.enable = true;
      # lxc.enable = true;
      # libvirtd.enable = true;
    };

    programs.gnupg.agent.enable = true;

    services.gnome.gnome-keyring.enable = true;

    home-manager.sharedModules = [
      {
        services.mako = {
          settings = {
            output = "eDP-1";
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

    services.acpid.handlers = {
      rfkill-ignore = {
        event = "button/rfkill.*";
        action = "/bin/true";
      };
    };
    services.logind.settings.Login = {
      HandleSwitchKey = "ignore";
    };

    boot = {
      loader = {
        systemd-boot.enable = true;
        efi.canTouchEfiVariables = true;
      };

      binfmt.emulatedSystems = [
        "aarch64-linux"
        "armv6l-linux"
      ];
      # s2idle suspend regression in 6.19.3+ (machine reboots instead of
      # resuming). Use 6.18 as fallback, switch back to latest once it
      # moves past the broken range. Bump the upper bound after testing
      # the "kernel-6_19-latest" specialisation on a new point release.
      kernelPackages =
        let
          latestVersion = pkgs.linuxPackages_latest.kernel.version;
          isBroken = lib.versionAtLeast latestVersion "6.19.3" && lib.versionOlder latestVersion "6.19.7";
        in
        lib.mkForce (if isBroken then pkgs.linuxPackages_6_18 else pkgs.linuxPackages_latest);
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
