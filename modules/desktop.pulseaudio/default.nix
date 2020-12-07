# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:
let
  user = config.myconfig.user;
  mb660_switch_profile = pkgs.writeShellScriptBin "mb660_switch_profile" ''
    export PATH=$PATH:${pkgs.pulseaudio}/bin:${pkgs.bash}/bin
    ${./bin/switch_sennheiser_profile}
  '';
  connectBtDevice = { name, id }:
    (pkgs.writeShellScriptBin (name + "_connect") ''
      # docu:
      #  see: https://wiki.archlinux.org/index.php/Bluetooth
      # - Start the bluetoothctl interactive command.
      #   - Enter power on to turn the power to the controller on. It is off by default and will turn off again each reboot, see #Auto power-on after boot.
      #   - Enter devices to get the MAC Address of the device with which to pair.
      #   - Enter device discovery mode with scan on command if device is not yet on the list.
      #   - Turn the agent on with agent on or choose a specific agent: if you press tab twice after agent you should see a list of available agents, e.g. DisplayOnly KeyboardDisplay NoInputNoOutput DisplayYesNo KeyboardOnly off on.
      #   - Enter pair MAC_address to do the pairing (tab completion works).
      #   - If using a device without a PIN, one may need to manually trust the device before it can reconnect successfully. Enter trust MAC_address to do so.
      #   - Enter connect MAC_address to establish a connection.

      id=${id}

      set -e

      unpair() {
          ${pkgs.bluez}/bin/bluetoothctl <<EOF
      untrust $id
      remove $id
      EOF
      }

      pair() {
          (sleep 1;
          echo "agent on";
          sleep 1;
          echo "scan on";
          sleep 10;
          echo "scan off";
          sleep 1;
          echo "pair $id";
          sleep 4;
          echo "trust $id";
          sleep 4;
          echo "info $id";
          echo "exit";
          ) |  ${pkgs.bluez}/bin/bluetoothctl
      }

      connect() {
          (sleep 1;
          echo "connect $id"
          sleep 2;
          echo "info $id";
          echo "exit";
          ) |  ${pkgs.bluez}/bin/bluetoothctl
      }

      if [[ "$1" == "--unpair" ]]; then
          shift
          unpair
          sleep 5
      fi
      if [[ "$1" == "--pair" ]]; then
          shift
          pair
          sleep 5
      fi
      connect
    '');

  pactl-monitor = pkgs.writeShellScriptBin "pactl-monitor" ''
    set -e
    pactl_monitor_file=/tmp/.pactl_monitor_file
    if [[ ! -f $pactl_monitor_file ]]; then
      touch $pactl_monitor_file
      ${pkgs.pulseaudio}/bin/pactl load-module module-loopback latency_msec=1
      echo "enable loopback"
    else
      rm $pactl_monitor_file
      ${pkgs.pulseaudio}/bin/pactl unload-module module-loopback || true
      echo "disable disable"
    fi
  '';
in {
  config = (lib.mkIf config.services.xserver.enable {
    home-manager.users."${user}" = {
      home.packages = with pkgs; [
        pavucontrol
        pamix
        mb660_switch_profile
        pulseeffects
        pactl-monitor
      ];
    };

    hardware.pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
      extraConfig =
        "load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1";
    };
    nixpkgs.overlays =
      [ (self: super: { helper = { inherit connectBtDevice; }; }) ];
    nixpkgs.config.pulseaudio = true;
  });
}
