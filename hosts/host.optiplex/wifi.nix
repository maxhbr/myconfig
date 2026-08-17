# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Built-in Wi-Fi on the Dell OptiPlex 3000 TC.
#
# TODO: confirm WiFi chip (the product page only says "wifi"; likely an
# Intel AX201 — check `lspci -k | grep -i -A3 network` once the box is up).
#
# Nothing chip-specific needs to be configured for now:
#   - the iwd backend (`myconfig.wifi.backend` defaults to "iwd") is
#     enabled by the base nixos.networking module on all hosts;
#   - the iwlwifi driver ships with the kernel and its firmware with
#     linux-firmware (`hardware.enableRedistributableFirmware` is set in
#     the flake core modules).
#
# Once the chip and the home SSID are confirmed, add a declarative
# NetworkManager profile here so the host connects automatically on boot
# (see hosts/host.odroid/wifi.nix). The passphrase can stay blank here and
# be overridden by the private repo via
# `networking.networkmanager.ensureProfiles.profiles.<ssid>.wifi-security.psk`.
{ ... }:
{ }
