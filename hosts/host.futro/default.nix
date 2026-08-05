# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Fujitsu Futro S740 thin client (Intel Celeron J4105, 4GB DDR4, 240GB M.2).
#
# Role: lightweight, remote-reachable headless node on the home LAN. It is
# primarily wired (eno1, DHCP) and runs eternal-terminal for remote shell
# access. There is no desktop, no Bluetooth, no sound, no printing.
#
# Memory (~4 GB RAM) is still constrained, so zram swap is enabled; the disk
# is now generous enough that no closure-size minimisation is needed and
# multiple old generations can coexist.
{
  config,
  pkgs,
  lib,
  myconfig,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
    # NB: eno1 is intentionally left on DHCP (no `metadatalib.fixIp`). A static
    # address via `networking.interfaces.eno1` would make NetworkManager report
    # eno1 as "connected (externally)"; the Wi-Fi arbitration in
    # `myconfig.networking.preferWired` is now driven from rtnetlink events and
    # copes with either, but DHCP keeps eno1 NM-managed and the config simpler.
    {
      services.eternal-terminal = {
        enable = true;
        port = 22022;
      };
      networking.firewall.allowedTCPPorts = [ 22022 ];
      networking.firewall.allowedUDPPorts = [ 22022 ];
    }
  ];

  config = {
    # Use the systemd-boot EFI boot loader.
    boot.loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      grub.enable = false;
    };

    myconfig = {
      desktop.enable = false;
      headless.enable = true;
      # eno1 and the USB Wi-Fi dongle both land on 192.168.1.0/24, so with
      # both up the box is dual-homed on one subnet and the Wi-Fi IP is
      # unreachable from the LAN (replies for it egress eno1 → asymmetric
      # path + ARP flux). This host is wired-primary with Wi-Fi as failover,
      # so drop Wi-Fi whenever the wired link has carrier and bring it back
      # when eno1 goes down.
      networking.preferWired.enable = true;
      observability = {
        client.enable = true;
        client.co2Exporter = {
          enable = true;
          room = "Schlafzimmer";
        };
      };
    };

    networking.hostName = "futro";
    networking.hostId = "fdb6854c";
    networking.networkmanager.enable = true; # kept for USB WiFi dongle support

    ##########################################################################
    # Memory
    ##########################################################################

    # zram creates a compressed RAM block device used as swap. On a 4 GB
    # box this is the single biggest memory win: it effectively multiplies
    # usable RAM and avoids touching the disk for swap. Sized to half of
    # RAM so zram itself can't starve active workloads.
    zramSwap = {
      enable = true;
      algorithm = "zstd";
      memoryPercent = 50;
    };

    # boot.tmp.useTmpfs is deliberately *not* enabled: on a 4 GB box a
    # tmpfs-backed /tmp can OOM the system when something writes a large
    # temp file. Keeping /tmp on the ext4 root is the safer trade-off here.

    system.stateVersion = lib.mkForce "26.11";
  };
}
