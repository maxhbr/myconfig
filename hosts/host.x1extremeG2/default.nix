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
    ./hardware.hantek
    ./mykeylight
    # ./backup-hdd.nix
    # ./foto-hdd.nix
    # modules
    ./role.work
    ./mail
    ## fun
    ./smarthome.nix
    ../host.workstation/gaming/games.steam
    {
      # Fingerprint reader: login and unlock with fingerprint (if you add one with `fprintd-enroll`)
      # services.fprintd.enable = true;
    }
  ];

  config = {
    myconfig = {
      desktop.enable = true;
      # virtualisation.enable = true;
      imagework.enable = true;
      cad.enable = true;
      dev = {
        compliance.enable = true;
        go.enable = true;
        haskell.enable = true;
        network.enable = true;
        nodejs.enable = true;
        ruby.enable = true;
        rust.enable = true;
      };
    };
    services.xserver.desktopManager.xfce.enable = true;
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

    services.openssh = {
      listenAddresses = [{
        addr = "127.0.0.1";
        port = 22;
      }];
    };

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
          ".config/autorandr/postswitch.d/unload_noisetorch".source = let
            script = with pkgs;
              writeShellScriptBin "script" ''
                exec ${noisetorch}/bin/noisetorch -u
              '';
          in "${script}/bin/script";
        };
      })
    ];
  };
}
