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
#
# Escape hatch: `sudo force-wlan on` suspends the policy and keeps Wi-Fi up
# even while wired has carrier (useful for testing/roaming); `sudo force-wlan
# off` resumes it, `force-wlan status` reports the current state. The override
# is persisted under /var/lib so it survives reboots: once you force Wi-Fi on
# it stays on across reboots until you explicitly `force-wlan off`. This is
# also what re-enables Wi-Fi early each boot despite NetworkManager and
# systemd-rfkill restoring a persisted "radio off" state from a prior session.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.networking.preferWired;

  # Persistent override flag. When this file exists the prefer-wired policy is
  # suspended and Wi-Fi is kept on regardless of the wired carrier. Lives under
  # /var/lib (not tmpfs) so `force-wlan on` survives reboots -> Wi-Fi is
  # re-enabled on every boot until you explicitly `force-wlan off`.
  stateDir = "/var/lib/prefer-wired";
  forceFlag = "${stateDir}/force-wlan";

  # `force-wlan [on|off|status]`: manual escape hatch to keep Wi-Fi up while a
  # wired uplink is present (e.g. you want both links for testing). Needs root
  # because `nmcli radio` and writing to the service RuntimeDirectory are
  # privileged -> run via sudo.
  forceWlan = pkgs.writeShellApplication {
    name = "force-wlan";
    runtimeInputs = [
      pkgs.networkmanager
      pkgs.coreutils
      pkgs.systemd
    ];
    text = ''
      flag=${forceFlag}
      cmd="''${1:-on}"
      case "$cmd" in
        on)
          mkdir -p "$(dirname "$flag")"
          touch "$flag"
          nmcli radio wifi on || true
          echo "force-wlan: ON (Wi-Fi kept on regardless of wired link)"
          ;;
        off)
          rm -f "$flag"
          echo "force-wlan: OFF (prefer-wired policy resumes)"
          # Re-seed the decision immediately so Wi-Fi is dropped again right
          # away if a wired link currently has carrier.
          systemctl restart prefer-wired.service 2>/dev/null || true
          ;;
        status)
          if [ -e "$flag" ]; then
            echo "force-wlan: ON (override active)"
          else
            echo "force-wlan: OFF (prefer-wired policy active)"
          fi
          nmcli -t radio wifi
          ;;
        *)
          echo "usage: force-wlan [on|off|status]" >&2
          exit 1
          ;;
      esac
    '';
  };
in
{
  options.myconfig.networking.preferWired = {
    enable = lib.mkEnableOption "myconfig.networking.preferWired";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ forceWlan ];

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
        # /var/lib/prefer-wired holds the persistent force-wlan override flag
        # (see below). StateDirectory survives reboots (unlike RuntimeDirectory).
        StateDirectory = "prefer-wired";
      };
      script = ''
        set -u

        # Manual override: if force-wlan has been engaged, keep Wi-Fi on and
        # do not arbitrate against the wired link at all.
        force_wlan_active() {
          [ -e ${forceFlag} ]
        }

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
          if force_wlan_active; then
            if [ "$current" = "disabled" ]; then
              echo "force-wlan active -> enabling Wi-Fi"
              nmcli radio wifi on || true
            fi
            return
          fi
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
