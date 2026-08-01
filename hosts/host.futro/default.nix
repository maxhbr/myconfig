# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Fujitsu Futro S740 thin client (Intel Celeron J4105, 4GB DDR4, 8GB M.2).
#
# Role: lightweight, remote-reachable headless node on the home LAN. It is
# primarily wired (eno1, DHCP) and runs eternal-terminal for remote shell
# access. There is no desktop, no Bluetooth, no sound, no printing — the
# system is tuned for minimal disk and memory footprint.
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
      # Only 8 GB of disk: keep very few boot entries so old kernels/initrds
      # don't accumulate on the small /boot partition. A switch transiently
      # needs room for both the old and new system closures, so keep this as
      # low as is still comfortable for rollback (3 entries).
      systemd-boot.configurationLimit = lib.mkForce 3;
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
      # `modules/nixos.nix-index.nix` enables nix-index (+ comma) for every
      # host. The prebuilt nix-index database (`index-x86_64-linux`) is
      # ~92 MB and useless on a headless thin client that is only ever
      # administered over SSH. Force it off here.
      {
        programs.nix-index.enable = lib.mkForce false;
        programs.nix-index-database.comma.enable = lib.mkForce false;
      }
    ];

    # `flake.lib.nix` sets `nix.nixPath` to pin the nixpkgs/home-manager/nixos
    # source trees into the system closure so `<nixpkgs>` works for ad-hoc
    # `nix-shell`. The nixpkgs source tree alone is ~201 MB. This box is
    # flake-only and administered over SSH; drop the channel-style NIX_PATH
    # to reclaim that space. (`nix-shell '<nixpkgs>'` and the `nixTest` fish
    # helper won't work here, which is fine for a thin client.)
    nix.nixPath = lib.mkForce [ ];
    # The flake framework also pins `nixpkgs` into `/etc/nix/registry.json`
    # (again the ~201 MB nixpkgs source tree). Clear the registry so that
    # store path isn't a GC root of the system either. `nixos-rebuild
    # switch --flake` doesn't use the registry; only `nix run nixpkgs#...`
    # style indirect lookups do, which this box doesn't need.
    nix.registry = lib.mkForce { };

    # The Futro S740 has no built-in WiFi/Bluetooth adapter, so the full
    # redistributable firmware bundle (linux-firmware, sof-firmware,
    # alsa-firmware, ipw2200, rtl8192su, … — ~800 MB closure) is dead weight
    # on this 8 GB box and stays disabled. Intel CPU microcode is a *separate*
    # option (`hardware.cpu.intel.updateMicrocode`, kept enabled below) and is
    # unaffected.
    hardware.enableRedistributableFirmware = lib.mkForce false;

    # This box is meant to host a USB WiFi dongle based on the MediaTek
    # MT7601U chipset (USB id 148f:7601). The in-tree `mt7601u` driver is a
    # stock loadable module that udev auto-loads on plug-in (via the module's
    # USB device-id table), so it needs no `boot.kernelModules` entry — but it
    # does need one firmware blob to bring the interface up. Without it the
    # driver binds and then fails with
    #   `Direct firmware load for mt7601u.bin failed`,
    # and the interface never comes up.
    #
    # The blob ships inside the full `linux-firmware` bundle (the ~800 MB we
    # just disabled above). To keep the disk win, ship *only* this single
    # ~44 KB blob instead of re-enabling the whole bundle.
    #
    # Firmware request path (kernel 6.18, drivers/net/wireless/mediatek/
    # mt7601u/usb.h): `#define MT7601U_FIRMWARE "mt7601u.bin"` — the driver
    # requests the *flat* `mt7601u.bin`, NOT `mediatek/mt7601u.bin`. Upstream
    # `linux-firmware` therefore stores the real blob at
    # `lib/firmware/mediatek/mt7601u.bin` and adds a flat relative symlink
    # `lib/firmware/mt7601u.bin -> mediatek/mt7601u.bin` (older kernels
    # requested the `mediatek/` path, which is why both exist). We mirror that
    # exact layout here so the blob loads regardless of which request path the
    # kernel uses, while pulling only this one file (plus a symlink) into the
    # runtime closure — not the ~800 MB bundle.
    #
    # NixOS merges `hardware.firmware` into `/run/current-system/firmware`
    # via buildEnv, after compressing each blob with zstd (default for kernels
    # >= 5.19). The compress step produces `mediatek/mt7601u.bin.zst` (the real
    # compressed blob) and rewrites the flat symlink to
    # `mt7601u.bin.zst -> mediatek/mt7601u.bin.zst`; both are resolved
    # transparently by the kernel's zstd firmware loader when the driver asks
    # for `mt7601u.bin`.
    hardware.firmware = [
      (pkgs.runCommand "mt7601u-firmware" { } ''
        install -Dm444 \
          ${pkgs.linux-firmware}/lib/firmware/mediatek/mt7601u.bin \
          $out/lib/firmware/mediatek/mt7601u.bin
        ln -s mediatek/mt7601u.bin $out/lib/firmware/mt7601u.bin
      '')
    ];

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

    # Aggressive GC for a ~7 GB store partition: keep only three days of
    # generations. `auto-optimise-store` is already on globally; the
    # min-free/max-free thresholds are tightened here so the daemon frees
    # space automatically (deleting the oldest garbage down to `max-free`
    # free bytes) whenever the store drops below `min-free` during a build
    # or a `nixos-rebuild switch` copy.
    nix.gc = {
      automatic = true;
      dates = lib.mkForce "03:15";
      options = lib.mkForce "--delete-older-than 3d";
      # Run the missed timer on next boot if the box was off at 03:15.
      persistent = true;
    };
    nix.extraOptions = ''
      min-free = ${toString (512 * 1024 * 1024)}
      max-free = ${toString (2 * 1024 * 1024 * 1024)}
    '';

    # ------------------------------------------------------------------
    # ONE-TIME on-device recovery (run these on the box over SSH *now* to
    # break a full-disk deadlock before this config can be deployed):
    #
    #   # 1. delete all old system/user generations and their store paths
    #   sudo nix-collect-garbage -d
    #   # 2. hard-link identical files in the store to reclaim duplicates
    #   sudo nix-store --optimise
    #   # 3. prune old boot entries (systemd-boot) if /boot is tight
    #   sudo /run/current-system/bin/switch-to-configuration boot
    #
    # After that the store should have enough headroom to realise + copy
    # the new (smaller) closure and `nixos-rebuild switch` will succeed.
    # ------------------------------------------------------------------

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
