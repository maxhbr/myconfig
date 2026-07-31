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
#   1. A NetworkManager dispatcher script that decisively *drops Wi-Fi while a
#      wired link has carrier* (and brings it back when wired drops). One
#      active uplink at a time → no dual-homing, no route fight, no rp_filter
#      drops.
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

  # Decides Wi-Fi radio state from the current wired carrier state and toggles
  # it only when it actually needs to change (idempotent → no event storms).
  #
  # NetworkManager invokes dispatcher scripts with:
  #   $1 = interface name, $2 = action (up/down/...)
  preferWired = pkgs.writeShellScript "nm-prefer-wired" ''
    set -eu
    iface="''${1:-}"
    action="''${2:-}"

    # Ignore events originating from Wi-Fi itself: toggling the Wi-Fi radio
    # below would otherwise recurse back into this script.
    if [ -n "$iface" ] && [ -d "/sys/class/net/$iface/wireless" ]; then
      exit 0
    fi

    # Only react to link-state transitions.
    case "$action" in
      up | down | vpn-up | vpn-down | connectivity-change) ;;
      *) exit 0 ;;
    esac

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

    current="$(${pkgs.networkmanager}/bin/nmcli -t radio wifi 2>/dev/null || echo unknown)"

    if wired_link_up; then
      if [ "$current" = "enabled" ]; then
        logger -t nm-prefer-wired "wired link up ($iface/$action) -> disabling Wi-Fi"
        ${pkgs.networkmanager}/bin/nmcli radio wifi off || true
      fi
    else
      if [ "$current" = "disabled" ]; then
        logger -t nm-prefer-wired "no wired link ($iface/$action) -> enabling Wi-Fi"
        ${pkgs.networkmanager}/bin/nmcli radio wifi on || true
      fi
    fi
  '';
in
{
  options.myconfig.networking.preferWired = {
    enable = lib.mkEnableOption "myconfig.networking.preferWired";
  };

  config = lib.mkIf cfg.enable {
    networking.networkmanager.dispatcherScripts = [
      {
        source = preferWired;
        type = "basic";
      }
    ];

    # Loose reverse-path filtering. Hosts that set net.ipv4.ip_forward = 1
    # (see modules/nixos.networking/default.nix) would otherwise have strict
    # rp_filter drop asymmetrically-routed replies during the (un)plug window.
    boot.kernel.sysctl = {
      "net.ipv4.conf.all.rp_filter" = lib.mkForce 2;
      "net.ipv4.conf.default.rp_filter" = lib.mkForce 2;
    };
  };
}
