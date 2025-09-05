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
{
  imports = [
    ./specialisation.nix
    ./hardware-configuration.nix
    inputs.nixos-hardware.nixosModules.common-pc-laptop
    inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
    inputs.nixos-hardware.nixosModules.framework-amd-ai-300-series
    ../../hardware/efi.nix
    ../../hardware/notebook-generic.nix
    ../../hardware/Radeon890M.nix
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
      services.eternal-terminal = {
        enable = true;
        port = 22022;
      };
      networking.firewall.interfaces."wg0".allowedTCPPorts = [ 22022 ];
      networking.firewall.interfaces."wg0".allowedUDPPorts = [ 22022 ];
    }
    { programs.kdeconnect.enable = true; }
    {
      # set CPU to performance mode
      boot.kernelParams = [ "amd_pstate=active" ];
      services.power-profiles-daemon.enable = true;
      # systemd.services.set-performance = {
      #   description = "Set performance profile";
      #   wantedBy = [ "multi-user.target" ];
      #   serviceConfig.Type = "oneshot";
      #   serviceConfig.ExecStart =
      #     "${config.services.power-profiles-daemon.package}/bin/powerprofilesctl set performance";
      # };
    }
  ];

  config = {
    networking = {
      hostName = "f13";
      hostId = "00000${config.networking.hostName}";
      useDHCP = false;
      interfaces.wlp192s0.useDHCP = true;

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
        btrfs_device = "/dev/disk/by-uuid/78c33ad0-409f-4ea5-9fe0-3050b9561788";
        btrbk_device = "/dev/disk/by-uuid/8e3c7395-c663-4080-9463-3b8a18bd7ad3";
        btrbk_luks_device = "/dev/disk/by-uuid/51df4369-0063-446a-96db-6d08b6349935";
        btrbk_luks_keyfile = "/home/mhuber/.password-store/hardware/backup/backupLuks_aa3f2db7-7b1a-4232-9b42-15270e9e5a79.key";
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
        imagework.myphoto.enable = false;
        cad.enable = true;
        joplin-desktop.enable = true;
        logseq.enable = true;
      };
      email = {
        enable = true;
        clients = [
          "aerc"
          "neomutt"
          "thunderbird"
          "himalaya"
        ];
      };
      virtualisation.enable = true;
      # editor.emacs.enable = false;
      dev = {
        # compliance.enable = true;
        # go.enable = false;
        # haskell.enable = true;
        network.enable = true;
        # nodejs.enable = true;
        # ruby.enable = true;
        # python.enable = true;
        # rust.enable = true;
        # elixir.enable = false;
        # zephyr.enable = true;
      };
    };
    virtualisation = {
      # docker.enable = true;
      podman.enable = true;
      # virtualbox.host.enable = true;
      # lxc.enable = true;
      # libvirtd.enable = true;
    };

    programs.evolution.enable = true;

    services.xserver.wacom.enable = false;
    services.xserver.digimend.enable = false;

    programs.gnupg.agent.enable = true;

    services.hardware.bolt.enable = true;

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
    system.stateVersion = lib.mkForce "25.05"; # Did you read the comment?
  };
}
