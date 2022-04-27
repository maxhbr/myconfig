{ config, lib, pkgs, ... }:
let
  mb660_switch_profile = pkgs.writeShellScriptBin "mb660_switch_profile" ''
    export PATH=$PATH:${config.hardware.pulseaudio.package}/bin:${pkgs.bash}/bin
    ${builtins.readFile ./bin/switch_sennheiser_profile}
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
in {
  config = (lib.mkIf config.hardware.bluetooth.enable {
    # hardware.bluetooth = {
    #   settings = { General = { Enable = "Source,Sink,Media,Socket"; }; };
    # };
    # see:
    # - https://github.com/NixOS/nixpkgs/issues/113628
    # - https://github.com/NixOS/nixpkgs/pull/113600
    systemd.services.bluetooth.serviceConfig.ExecStart = [
      ""
      "${pkgs.bluez}/libexec/bluetooth/bluetoothd -f /etc/bluetooth/main.conf"
    ];
    home-manager.sharedModules = [
      (lib.mkIf config.hardware.pulseaudio.enable {
        home.packages = with pkgs; [ mb660_switch_profile ];

        programs.fish = {
          functions = {
            # see: https://nixos.wiki/wiki/Bluetooth
            pactl-bt-to-a2dp =
              "${config.hardware.pulseaudio.package}/bin/pacmd set-card-profile (${config.hardware.pulseaudio.package}/bin/pactl list cards short | ${pkgs.gnugrep}/bin/egrep -o bluez_card[[:alnum:]._]+) a2dp_sink";
          };
        };
      })
    ];
    nixpkgs.overlays =
      [ (self: super: { helper = { inherit connectBtDevice; }; }) ];
  });
}
