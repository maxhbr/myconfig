# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# WiFi via the Realtek RTL8192CU USB dongle (rtl8192cu driver).
#
# The firmware (rtlwifi/rtl8192cufw_TMSC.bin) ships with linux-firmware and is
# already available because hardware-configuration.nix enables
# `hardware.enableRedistributableFirmware`.
#
# A declarative NetworkManager profile is created for the "QS4j" network so the
# host connects automatically on boot. The passphrase is intentionally left
# blank here; the private repo overrides `...wifi-security.psk` with the real
# value, e.g.:
#
#   networking.networkmanager.ensureProfiles.profiles.QS4j.wifi-security.psk =
#     "<real-passphrase>";
#
# Alternatively use `ensureProfiles.environmentFiles` and reference an env var
# such as `psk = "$QS4J_PSK";`.
{ ... }:
{
  networking.networkmanager.ensureProfiles.profiles.QS4j = {
    connection = {
      id = "QS4j";
      type = "wifi";
      autoconnect = true;
    };
    wifi = {
      ssid = "QS4j";
      mode = "infrastructure";
    };
    wifi-security = {
      key-mgmt = "wpa-psk";
      psk = "";
    };
    ipv4.method = "auto";
    ipv6.method = "auto";
  };
}
