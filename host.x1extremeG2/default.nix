# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let user = config.myconfig.user;
in {
  imports = [
    ./hardware-configuration.nix
    ../modules
    ./myconfig-master
    # hardware:
    ../hardware/x1extremeG2.nix
    ../hardware/efi.nix
    ../hardware/footswitch.nix
    ../hardware/steamcontroller.nix
    ../hardware/blink1.nix
    # ./backup-hdd.nix
    # ./foto-hdd.nix
    # modules
    ./role.work
    ./mail
    ## fun
    ./smarthome.nix
    {
      # Fingerprint reader: login and unlock with fingerprint (if you add one with `fprintd-enroll`)
      services.fprintd.enable = true;
    }
  ] ++ (with (import ../lib.nix); [
    (setupAsWireguardClient "10.199.199.2")
    # (setupNasNFS "bilder")
    # (setupNasNFS "data")
    # (announceHost "workstation" [ ])
    # (announceHost "nas" [ "monitoring" "grafana" "prometheus" "deconz" ])
    # (announceHost "vserver" [ ])
    # (announceHost "nuc" [ ])
    # # (announceHost "pi0" [])
    # (announceHost "pi3a" [ ])
    # (announceHost "pi4" [ ])
    (lib.mkIf config.virtualisation.lxc.enable { # nat for lxc
      networking = {
        nat = {
          enable = true;
          internalInterfaces = [ "ve-+" ];
          externalInterface = "enp0s31f6";
        };
        networkmanager.unmanaged = [ "interface-name:ve-*" ];
      };
    })
  ]);

  config = {
    myconfig = {
      desktop.enable = true;
      # virtualisation.enable = true;
      imagework.enable = true;
      dev = {
        haskell.enable = true;
        iot.enable = true;
        network.enable = true;
        compliance.enable = true;
      };
    };
    services.xserver.wacom.enable = true;
    virtualisation.docker.enable = true;
    virtualisation.libvirtd.enable = true;
    # virtualisation.virtualbox.host.enable = true;

    networking.hostName = "x1extremeG2";
    networking.hostId = "7634ddfe";

    boot.initrd.supportedFilesystems = [ "luks" ];
    boot.initrd.luks.devices.crypted = {
      device = "/dev/disk/by-uuid/2118a468-c2c3-4304-b7d3-32f8e19da49f";
      preLVM = true;
      allowDiscards = true;
    };

    services.openssh = {
      listenAddresses = [{
        addr = "127.0.0.1";
        port = 22;
      }];
    };

    home-manager.users."${user}" = {
      # home.packages = with pkgs.helper; [
      #   (connectBtDevice {
      #     name = "mb660";
      #     id = "00:16:94:42:53:10";
      #   })
      #   (connectBtDevice {
      #     name = "5200";
      #     id = "E4:22:A5:3E:F4:3D";
      #   })
      #   (connectBtDevice {
      #     name = "klim";
      #     id = "1E:A8:2C:18:00:3D";
      #   })
      #   (connectBtDevice {
      #     name = "wm25";
      #     id = "03:A1:00:01:7B:13";
      #   })
      # ];
      home.file = {
        ".config/autorandr/" = {
          source = ./autorandr;
          recursive = true;
        };
        ".config/autorandr/postswitch.d/mute_notebook_audio".source = let
          muteNotebookAudio = with pkgs;
            writeShellScriptBin "mute_notebook_audio" ''
              exec ${pulseaudio}/bin/pactl set-sink-mute "alsa_output.pci-0000_00_1f.3.analog-stereo" "1"
            '';
        in "${muteNotebookAudio}/bin/mute_notebook_audio";
      };
      home.packages = with pkgs.unstable;
        [
          google-chrome # for netflix and stadia
        ];
    };
  };
}
