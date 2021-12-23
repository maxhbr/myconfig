# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, myconfig, ... }: {
  imports = [
    ./hardware-configuration.nix
    ../../hardware/efi.nix
    ../../hardware/btrfs.nix
    ../../hardware/nixos-hardware/common/cpu/amd
    ../../hardware/nixos-hardware/common/pc/ssd
    ../../hardware/hdd-spinndown.nix
    ../../hardware/steamcontroller.nix
    ./4x500-hdds.raid.nix
    (myconfig.metadatalib.fixIp "enp39s0")
    (myconfig.metadatalib.setupAsBuildMachine [
      myconfig.metadatalib.get.hosts.x1extremeG2.pubkeys."id_ed25519.pub"
      myconfig.metadatalib.get.hosts.x1extremeG2.pubkeys."id_rsa.pub"
    ])
    # other profiles
    ./gaming
    # testing
    ./chrootPopOS.nix
    { # for quickfix (due to usage of 20.03)
      nixpkgs.config.allowBroken = true;
    }
    {
      environment.systemPackages = with pkgs; [ x11vnc ];
      ## Setup via ssh tunnel:
      # $ ssh -t -L 5900:localhost:5900 $IP 'x11vnc -ncache 10 -unixpw -localhost -display :0'
      ## in other terminal:
      # $ vncviewer -encodings 'copyrect tight zrle hextile' localhost:0

      ## or open ports
      # networking.firewall.allowedUDPPorts = [ 5900 ];
      # networking.firewall.allowedTCPPorts = [ 5900 ];
    }
    {
      services.xrdp.enable = true;
      services.xrdp.defaultWindowManager = "${pkgs.icewm}/bin/icewm";
      networking.firewall.allowedTCPPorts = [ 3389 ];
    }
    ( # wol
      let interface = "enp39s0";
      in {
        # services.wakeonlan.interfaces = [{
        #   inherit interface;
        #   method = "magicpacket";
        # }];

        # [Unit]
        # Description=Wake-on-LAN for %i
        # Requires=network.target
        # After=network.target

        # [Service]
        # ExecStart=/usr/bin/ethtool -s %i wol g
        # Type=oneshot

        # [Install]
        # WantedBy=multi-user.target
        systemd.services.wolEnable = {
          description = "Wake-on-LAN for ${interface}";
          requires = [ "network.target" ];
          after = [ "network.target" ];
          wantedBy = [ "multi-user.target" ];

          serviceConfig = {
            ExecStart = "${pkgs.ethtool}/bin/ethtool -s ${interface} wol g";
            Type = "oneshot";
          };
        };
      })
  ]; # ++ (with (import ../lib.nix); [ (setupAsWireguardClient "10.199.199.5") ]);

  config = {
    networking.hostName = "workstation";
    networking.hostId = "864d73f4";
    myconfig = {
      desktop.enable = true;
      headless.enable = true;
      # virtualisation.enable = true;
      # virtualisation.gpuPassthroughHost.enable = true;
      imagework.enable = true;
      dev = {
        haskell.enable = true;
        compliance.enable = true;
        network.enable = true;
      };
    };
    virtualisation.podman.enable = true;
    virtualisation.docker.enable = false;

    #boot.kernelPackages =
    #  # lib.mkForce pkgs.unstable.linuxPackages_testing;
    #  # lib.mkForce pkgs.linuxPackages_testing;
    #  # lib.mkForce pkgs.unstable.linuxPackages_latest;
    #  lib.mkForce pkgs.linuxPackages_latest;

    services.logind.extraConfig = ''
      HandlePowerKey=suspend
    '';

    hardware.enableRedistributableFirmware = true;
    hardware.cpu.amd.updateMicrocode = true;
    services.xserver = {
      xrandrHeads = [{
        output = "HDMI-A-0";
        primary = true;
        monitorConfig = ''
          # 2560x1440 59.96 Hz (CVT 3.69M9) hsync: 89.52 kHz; pclk: 312.25 MHz
          Modeline "2560x1440_60.00"  312.25  2560 2752 3024 3488  1440 1443 1448 1493 -hsync +vsync
          Option "PreferredMode" "2560x1440"
        '';
      }];
      videoDrivers = [ "amdgpu" ];
    };

    boot.initrd.supportedFilesystems = [ "luks" "btrfs" ];
    boot.binfmt.emulatedSystems = [ "aarch64-linux" "armv6l-linux" ];

    fileSystems."/mnt/2tb-1" = {
      device = "/dev/disk/by-uuid/51d362d8-5b73-4b92-84c3-9ff260062da6";
      fsType = "ext4";
    };

    services.snapper = {
      snapshotInterval = "hourly";
      cleanupInterval = "1d";
      filters = null;
      configs = {
        home = {
          subvolume = "/home";
          extraConfig = ''
            ALLOW_USERS="${myconfig.user}"
          '';
        };
      };
    };

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = lib.mkForce "20.09"; # Did you read the comment?
  };
}
