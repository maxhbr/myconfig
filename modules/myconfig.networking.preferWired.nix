# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Opt-in policy for hosts that can end up dual-homed on the *same* subnet
# (e.g. a wired NIC and Wi-Fi both landing on 192.168.1.0/24 via DHCP). With
# no arbitration such a machine has:
#
#   * two default routes (wired + Wi-Fi) fighting over egress,
#   * return traffic arriving on the "wrong" interface (and, on hosts that
#     enable `net.ipv4.ip_forward` for WireGuard, the default *strict*
#     reverse-path filter then silently dropping those packets), and
#   * every carrier / DHCP / route change re-triggering WireGuard endpoint
#     roaming re-handshakes.
#
# The practical failure is that the Wi-Fi IP is unreachable from the LAN
# whenever the wired link is also up (replies for the Wi-Fi address egress the
# wired NIC → asymmetric path + ARP flux → inbound TCP never completes), plus
# several seconds of "flickering" connectivity on every (un)plug.
#
# This module fixes that with two measures:
#
#   1. A systemd service that decisively *drops Wi-Fi while a wired link has
#      carrier* (and brings it back when wired drops). One active uplink at a
#      time → no dual-homing, no route fight, no rp_filter drops.
#
#      The decision is driven from the kernel's rtnetlink link notifications
#      (`ip monitor link`) rather than from NetworkManager dispatcher events.
#      A dispatcher-only implementation misses hosts whose wired NIC is *not*
#      managed by NetworkManager — e.g. a statically addressed interface set
#      via `networking.interfaces.<iface>` (as `metadatalib.fixIp` does), which
#      NM reports as "connected (externally)" and never fires dispatcher
#      up/down events for. rtnetlink sees every interface's carrier changes
#      regardless of who manages it, so this works for both DHCP-managed and
#      externally/statically managed wired links.
#   2. Loosening the reverse-path filter to "loose" mode (rp_filter = 2) as
#      defense-in-depth for the brief transition window where both links are
#      momentarily up.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.networking.preferWired;
in
{
  options.myconfig.networking.preferWired = {
    enable = lib.mkEnableOption "myconfig.networking.preferWired";
  };

  config = lib.mkIf cfg.enable {
    systemd.services.prefer-wired = {
      description = "Disable Wi-Fi while a wired uplink has carrier";
      # NetworkManager owns the Wi-Fi radio (`nmcli radio wifi`), so we need it
      # running before we can toggle. `ip monitor` itself is pure netlink and
      # needs nothing.
      after = [ "NetworkManager.service" ];
      wants = [ "NetworkManager.service" ];
      wantedBy = [ "multi-user.target" ];
      path = [
        pkgs.iproute2
        pkgs.networkmanager
      ];
      serviceConfig = {
        # If NetworkManager is not ready yet at first start (nmcli fails), or
        # the monitor pipe ever dies, come straight back and re-seed the
        # decision.
        Restart = "always";
        RestartSec = 3;
      };
      script = ''
        set -u

        # Is any *physical wired* interface currently carrying a link?
        wired_link_up() {
          for d in /sys/class/net/*; do
            name="$(basename "$d")"
            # skip wireless
            [ -d "$d/wireless" ] && continue
            # skip virtual / tunnel / loopback interfaces
            case "$name" in
              lo | wg* | veth* | docker* | podman* | virbr* | tun* | tap* | br* | vboxnet*)
                continue
                ;;
            esac
            [ -e "$d/carrier" ] || continue
            if [ "$(cat "$d/carrier" 2>/dev/null || echo 0)" = "1" ]; then
              return 0
            fi
          done
          return 1
        }

        # Toggle the Wi-Fi radio only when it actually needs to change
        # (idempotent → wlan0's own flaps can't cause a toggle storm).
        apply() {
          current="$(nmcli -t radio wifi 2>/dev/null || echo unknown)"
          if wired_link_up; then
            if [ "$current" = "enabled" ]; then
              echo "wired link up -> disabling Wi-Fi"
              nmcli radio wifi off || true
            fi
          else
            if [ "$current" = "disabled" ]; then
              echo "no wired link -> enabling Wi-Fi"
              nmcli radio wifi on || true
            fi
          fi
        }

        # Seed the decision from the current state, then re-evaluate on every
        # rtnetlink link event (carrier flips flip IFF_LOWER_UP -> RTM_NEWLINK).
        apply
        ip monitor link | while read -r _line; do
          apply
        done
      '';
    };

    # Loose reverse-path filtering. Hosts that set net.ipv4.ip_forward = 1
    # (see modules/nixos.networking/default.nix) would otherwise have strict
    # rp_filter drop asymmetrically-routed replies during the (un)plug window.
    boot.kernel.sysctl = {
      "net.ipv4.conf.all.rp_filter" = lib.mkForce 2;
      "net.ipv4.conf.default.rp_filter" = lib.mkForce 2;
    };
  };
}
