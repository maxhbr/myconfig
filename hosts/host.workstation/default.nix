# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, myconfig, ... }: {
  imports = [
    ./hardware-configuration.nix
    ./imgwork.nfs.nix
    ../../hardware/efi.nix
    ../../hardware/btrfs.nix
    ../../hardware/nixos-hardware/common/cpu/amd
    ../../hardware/nixos-hardware/common/pc/ssd
    ../../hardware/steamcontroller.nix
    ({ ... }: {
      config = {
        services.ollama = {
          enable = true;
          # listenAddress = "0.0.0.0:11434";
          acceleration = "rocm";
          # environmentVariables = {
          #   OLLAMA_LLM_LIBRARY = "cpu";
          #   HIP_VISIBLE_DEVICES = "0,1";
          # };
        };
      };
    })
    (myconfig.metadatalib.fixIp "enp4s0")
    ( # wol
      let interface = "enp4s0";
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
    (myconfig.metadatalib.setupAsBuildMachine [
      myconfig.metadatalib.get.hosts.p14.pubkeys."id_ed25519_no_pw.pub"
      myconfig.metadatalib.get.hosts.x1extremeG2.pubkeys."id_ed25519.pub"
      myconfig.metadatalib.get.hosts.x1extremeG2.pubkeys."id_rsa.pub"
    ])
    {
      services.vsftpd = { enable = true; };
    }
    # other profiles
    ./gaming
  ]; # ++ (with (import ../lib.nix); [ (setupAsWireguardClient "10.199.199.5") ]);

  config = {
    networking.hostName = "workstation";
    networking.hostId = "864d73f4";
    myconfig = {
      desktop = {
        enable = true;
        xserver = {
          enable = true;
          xmonad.enable = false;
          kde.enable = true;
        };
        wayland = {
          enable = false;
          selectedSessions = [
            # "hyprland"
            "niri"
            "niri-plain"
            "labwc"
            "river"
            "plasma6"
             # "qtile"
          ];
        };
        myphoto.enable = true;
        obs.enable = true;
      };
      headless.enable = true;
      # virtualisation.gpuPassthroughHost.enable = true;
      dev = {
        compliance.enable = true;
        haskell.enable = true;
        network.enable = true;
      };
    };

    virtualisation = {
      docker.enable = true;
      podman.enable = true;
      oci-containers = { backend = "podman"; };
      #virtualbox.host.enable = true;
    };

    #services.physlock.enable = true;

    programs.dconf.enable = true;

    services.logind.extraConfig = ''
      HandlePowerKey=suspend
      RuntimeDirectorySize=6G
    '';

    hardware.enableRedistributableFirmware = true;
    hardware.cpu.amd.updateMicrocode = true;
    services.xserver = {
      videoDrivers = [ "amdgpu" ];
    };

    boot.initrd.supportedFilesystems = [ "luks" "btrfs" ];
    boot.binfmt.emulatedSystems = [ "aarch64-linux" "armv6l-linux" ];

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = lib.mkForce "20.09"; # Did you read the comment?
  };
}
