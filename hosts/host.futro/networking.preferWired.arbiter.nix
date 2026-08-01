# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Wi-Fi arbitration for the futro thin client.
#
# Root cause this fixes
# ---------------------
# `myconfig.networking.preferWired` (enabled in ./default.nix) is meant to keep
# this box single-homed: it should drop the Wi-Fi radio whenever the wired
# uplink (eno1) has carrier, so eno1 (192.168.1.171) and the Wi-Fi dongle don't
# both land on 192.168.1.0/24 at once. Dual-homing on one subnet gives the box
# two default routes, makes the Wi-Fi IP unreachable from the LAN (replies for
# it egress eno1 → asymmetric path + ARP flux), and re-triggers WireGuard
# endpoint roaming on every route change.
#
# That shared module implements the policy purely as a *NetworkManager
# dispatcher script*, i.e. it only ever runs in reaction to NM up/down events.
# On this host eno1 is given a static address through `metadatalib.fixIp`
# (`networking.interfaces.eno1.ipv4.addresses`), which NetworkManager sees as
# "connected (externally)" — it never assumes the connection and therefore
# never fires dispatcher up/down events for eno1. The dispatcher consequently
# only ever receives *wlan0* events, which it deliberately ignores to avoid
# recursion. Net result: the Wi-Fi radio is never turned off while eno1 is up,
# so the box stays dual-homed and its Wi-Fi address is unreachable from the LAN
# (observable symptom: `ssh <wifi-ip>` times out while `ssh <eno1-ip>` works,
# and `ip route` shows two competing default routes).
#
# (The sibling host f13 also enables `preferWired`, but its wired NIC is
# DHCP-managed by NetworkManager, so there the dispatcher *does* fire and the
# shared module works — which is why the defect is host-specific to futro.)
#
# The fix
# -------
# Drive the exact same "one uplink at a time" decision from an event source
# that does see eno1: the kernel's rtnetlink link notifications. A tiny
# long-running service seeds the decision once at start and then re-applies it
# on every link-state change reported by `ip monitor link`. The wired carrier
# is read authoritatively from sysfs, and the toggle is idempotent (guarded on
# the current radio state), so wlan0's own flaps can't cause a toggle storm.
{
  pkgs,
  ...
}:
{
  systemd.services.prefer-wired-arbiter = {
    description = "Disable Wi-Fi while the wired uplink (eno1) has carrier";
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
      # If NetworkManager is not ready yet at first start (nmcli fails), or the
      # monitor pipe ever dies, come straight back and re-seed the decision.
      Restart = "always";
      RestartSec = 3;
    };
    script = ''
      set -u

      apply() {
        wired_carrier="$(cat /sys/class/net/eno1/carrier 2>/dev/null || echo 0)"
        current="$(nmcli -t radio wifi 2>/dev/null || echo unknown)"
        if [ "$wired_carrier" = "1" ]; then
          if [ "$current" = "enabled" ]; then
            echo "eno1 has carrier -> disabling Wi-Fi"
            nmcli radio wifi off || true
          fi
        else
          if [ "$current" = "disabled" ]; then
            echo "eno1 lost carrier -> enabling Wi-Fi"
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
}
