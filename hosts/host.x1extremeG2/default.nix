# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }: {
  imports = [
    ./hardware-configuration.nix
    ./myconfig-master
    # hardware:
    ../../hardware/x1extremeG2.nix
    ../../hardware/efi.nix
    ../../hardware/footswitch.nix
    # ../../hardware/steamcontroller.nix
    ../../hardware/blink1.nix
    ../../hardware/unifying.nix
    ../host.p14/hardware.hantek
    ../host.p14/mykeylight
    # ./backup-hdd.nix
    # ./foto-hdd.nix
    # modules
    ../host.p14/role.work
    # ./service.docker-registry.nix
    # ./service.podman.buildkit.nix
    ../host.p14/mail
    {
      myconfig.river.enable = true;
      myconfig.qtile.enable = true;
      programs.sway.enable = true;
    }
    ## fun
    ./smarthome.nix
    ../host.workstation/gaming/games.steam
    ./services.xserver.programs.platformio.nix
    {
      # Fingerprint reader: login and unlock with fingerprint (if you add one with `fprintd-enroll`)
      # services.fprintd.enable = true;
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
  ];

  config = {
    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = lib.mkForce "22.11"; # Did you read the comment?

    myconfig = {
      desktop.enable = true;
      # virtualisation.enable = true;
      imagework.enable = true;
      cad.enable = true;
      deskreen.enable = true;
      dev = {
        compliance.enable = true;
        go.enable = true;
        haskell.enable = true;
        network.enable = true;
        nodejs.enable = true;
        # ruby.enable = true;
        rust.enable = true;
      };
    };
    # services.xserver.desktopManager.xfce.enable = true;
    services.xserver.wacom.enable = true;
    virtualisation.docker.enable = true;
    virtualisation.podman.enable = true;
    # virtualisation.libvirtd.enable = true;
    # virtualisation.virtualbox.host.enable = true;

    services.hardware.bolt.enable = true;

    # zramSwap.enable = true;

    networking.hostName = "x1extremeG2";
    networking.hostId = "7634ddfe";

    boot.initrd.supportedFilesystems = [ "luks" ];
    boot.initrd.luks.devices.crypted = {
      device = "/dev/disk/by-uuid/2118a468-c2c3-4304-b7d3-32f8e19da49f";
      preLVM = true;
      allowDiscards = true;
    };
    boot.initrd.luks.devices.crypted2 = {
      device = "/dev/disk/by-uuid/56c9fe80-4b6d-42dc-a025-0a2138ae012e";
      allowDiscards = true;
      preLVM = false;
      # keyFileSize = 4096;
      # # pinning to /dev/disk/by-id/usbkey works
      # keyFile = "/dev/sdb";
    };

    # services.openssh = {
    #   listenAddresses = [{
    #     addr = "127.0.0.1";
    #     port = 22;
    #   }];
    # };

    home-manager.sharedModules = [
      {
        home.file = {
          ".config/autorandr/" = {
            source = ./autorandr;
            recursive = true;
          };
          ".config/autorandr/mobile/postswitch.d/mykeylight-off".source = let
            script = with pkgs;
              writeShellScriptBin "script"
              "${mykeylight-off}/bin/mykeylight-off &disown";
          in "${script}/bin/script";
        };
        home.packages = with pkgs; [
          google-chrome # for netflix and stadia
          comma
        ];
      }
      (lib.mkIf config.hardware.pulseaudio.enable {
        home.file = {
          ".config/autorandr/postswitch.d/mute_notebook_audio".source = let
            script = with pkgs;
              writeShellScriptBin "script" ''
                exec ${pulseaudio}/bin/pactl set-sink-mute "alsa_output.pci-0000_00_1f.3.analog-stereo" "1"
              '';
          in "${script}/bin/script";
        };
      })
    ];
  };
}
