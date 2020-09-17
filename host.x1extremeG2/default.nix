# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }: {
  imports = [
    ./hardware-configuration.nix
    ../role.dev
    ../role.myconfig-master
    # hardware:
    ../hardware/x1extremeG2.nix
    ../hardware/efi.nix
    ../hardware/footswitch.nix
    ./backup-hdd.nix
    ./foto-hdd.nix
    # modules
    ../role.work
    ./mail
    ## fun
    ../role.imagework
    ../role.gaming/games.steam
    ../role.desktop/desktop.X.obs.nix
    ./smarthome.nix
  ] ++ (with (import ../lib.nix); [
    (setupAsWireguardClient "10.199.199.2")
    # (setupNasNFS "bilder")
    # (setupNasNFS "data")
    (announceHost "workstation")
    (announceHost "nas")
    (announceHost "vserver")
    (announceHost "nuc")
    # (announceHost "pi0")
    (announceHost "pi3a")
    (announceHost "pi4")
    (lib.mkIf config.virtualisation.lxc.enable
      { # nat for lxc
        networking = {
          nat = {
            enable = true;
            internalInterfaces = [ "ve-+" ];
            externalInterface = "enp0s31f6";
          };
          networkmanager.unmanaged = [ "interface-name:ve-*" ];
        };
      }
    )
  ]);

  config = {
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

    environment.systemPackages = with pkgs.helper; [
      (connectBtDevice {
        name = "mb660";
        id = "00:16:94:42:53:10";
      })
      (connectBtDevice {
        name = "5200";
        id = "E4:22:A5:3E:F4:3D";
      })
      (connectBtDevice {
        name = "klim";
        id = "1E:A8:2C:18:00:3D";
      })
      (connectBtDevice {
        name = "wm25";
        id = "03:A1:00:01:7B:13";
      })
    ];

    home-manager.users.mhuber = {
      home.file = {
        ".config/autorandr/" = {
          source = ./autorandr;
          recursive = true;
        };
      };
    };
  };
}
