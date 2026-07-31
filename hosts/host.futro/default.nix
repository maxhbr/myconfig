# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Fujitsu Futro S740 thin client (Intel Celeron J4105, 4GB DDR4, 8GB M.2).
#
# Role: lightweight, remote-reachable headless node on the home LAN. It is
# primarily wired (eno1, fixed IP 192.168.1.102) and runs eternal-terminal
# for remote shell access. There is no desktop, no Bluetooth, no sound,
# no printing — the system is tuned for minimal disk and memory footprint.
#
# Resource constraints (~4 GB RAM, ~8 GB disk) drive every setting below;
# each optimisation is annotated with its rationale.
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
    (myconfig.metadatalib.fixIp "eno1")
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
      # Only 8 GB of disk: keep very few boot entries so old kernels/initrds
      # don't accumulate on the small /boot partition.
      systemd-boot.configurationLimit = 5;
    };

    myconfig = {
      desktop.enable = false;
      headless.enable = true;
      observability = {
        client.enable = true;
        client.co2Exporter.enable = true;
      };
    };

    networking.hostName = "futro";
    networking.hostId = "fdb6854c";
    networking.networkmanager.enable = true; # kept for USB WiFi dongle support
    # The shared `nixos.networking` module adds `networkmanager-openvpn` as
    # a NM plugin, which pulls in gtk4 → gstreamer → ffmpeg → clang+llvm+gcc
    # (~1.6 GB closure). This thin client doesn't need OpenVPN via NM.
    networking.networkmanager.plugins = lib.mkForce [ ];
    # NetworkManager defaults to enabling ModemManager (libqmi, ~67 MB).
    # A WiFi-dongle-only box has no use for cellular modem management.
    networking.modemmanager.enable = false;

    ##########################################################################
    # Disk footprint — heavy services pulled in by shared modules
    ##########################################################################

    # `services.dbus.nix` unconditionally enables xdg-desktop-portal with
    # the wlr backend, which drags in pipewire → ffmpeg → clang+llvm+gcc
    # (~1.4 GB closure). A headless box has no use for desktop portals.
    xdg.portal.enable = lib.mkForce false;
    xdg.portal.wlr.enable = lib.mkForce false;

    # `modules/services.fwupd.nix` enables fwupd for all hosts. Firmware
    # updates on a thin client are unnecessary and the closure is ~400 MB.
    services.fwupd.enable = lib.mkForce false;

    # `myconfig.headless.enable` pulls in netdata (~345 MB closure, plus
    # ~50-100 MB RSS). Too heavy for a 4 GB box that is never monitored
    # locally.
    services.netdata.enable = lib.mkForce false;

    # `modules/programs.neovim/default.nix` unconditionally enables neovim
    # *and* neovide (a GUI Neovim client) plus copilot-vim (which needs
    # nodejs). We can't disable neovim entirely because the fallback vim
    # module uses `builtins.fetchurl` from ftp.vim.org at eval time (network
    # dependency). Instead, disable neovide (GUI, pulls in GTK) via
    # home-manager and let the copilot-vim nodejs dep stay — it's only
    # ~100 MB and removing it would require shared module changes.
    home-manager.sharedModules = [
      { programs.neovide.enable = lib.mkForce false; }
    ];

    # The Futro S740 has no built-in WiFi/Bluetooth adapter, so
    # the redistributable firmware bundle (linux-firmware, sof-firmware,
    # alsa-firmware, ipw2200, rtl8192su, … — ~800 MB closure) is dead
    # weight. Intel CPU microcode is a *separate* option
    # (`hardware.cpu.intel.updateMicrocode`, kept enabled) and is unaffected.
    hardware.enableRedistributableFirmware = lib.mkForce false;
    # Keep Intel CPU microcode updates (separate, tiny ~14 MB package).
    # hardware-configuration.nix defaults this to
    # `config.hardware.enableRedistributableFirmware`, so set it explicitly.
    hardware.cpu.intel.updateMicrocode = true;

    # btrfs/luks are not used by this host (root is ext4, see
    # hardware-configuration.nix). Drop them from the initrd so the
    # initrd doesn't pull in btrfs-progs, cryptsetup, etc.
    boot.initrd.supportedFilesystems = lib.mkForce [ "ext4" ];

    # Don't install man pages, info pages, or the NixOS HTML manual
    # (`nixos-help`, `nixos-manual-html`). On an 8 GB disk the combined
    # manual/man-pages closure is a meaningful chunk for a box that is
    # only ever administered over SSH.
    documentation.enable = false;
    documentation.man.enable = false;
    documentation.doc.enable = false;
    documentation.info.enable = false;
    # `core.nix` sets this to true at default priority.
    documentation.nixos.enable = lib.mkForce false;

    # command-not-found ships a per-channel sqlite index; useless on a
    # flake-only host and it wastes disk + eval time.
    programs.command-not-found.enable = false;

    # Cap journald so logs can't fill the 8 GB disk.
    services.journald.extraConfig = ''
      SystemMaxUse=100M
      RuntimeMaxUse=32M
      MaxRetentionSec=7day
    '';

    # Aggressive but safe GC for an 8 GB disk: keep only one week of
    # generations. `auto-optimise-store` is already on globally; the
    # min-free/max-free thresholds are tightened here so the store is
    # hard-deduplicated and trimmed when space gets tight.
    nix.gc = {
      automatic = true;
      dates = lib.mkForce "daily";
      options = lib.mkForce "--delete-older-than 7d";
    };
    nix.extraOptions = ''
      min-free = ${toString (256 * 1024 * 1024)}
      max-free = ${toString (1 * 1024 * 1024 * 1024)}
    '';

    ##########################################################################
    # Memory
    ##########################################################################

    # zram creates a compressed RAM block device used as swap. On a 4 GB
    # box this is the single biggest memory win: it effectively multiplies
    # usable RAM and avoids touching the slow 8 GB M.2 for swap. Sized to
    # half of RAM so zram itself can't starve active workloads.
    zramSwap = {
      enable = true;
      algorithm = "zstd";
      memoryPercent = 50;
    };

    # boot.tmp.useTmpfs is deliberately *not* enabled: on a 4 GB box a
    # tmpfs-backed /tmp can OOM the system when something writes a large
    # temp file. Keeping /tmp on the (small but adequate) ext4 root is the
    # safer trade-off here.

    system.stateVersion = lib.mkForce "26.11";
  };
}
