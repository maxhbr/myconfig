{
  config,
  lib,
  pkgs,
  ...
}:

let
  mkScript =
    bluez_devices: preferred_sinks_patterns: preferred_sources_patterns:
    pkgs.writeShellScriptBin "fix-audio" ''
      set -euo pipefail

      bluez_devices=(
        ${bluez_devices}
      )

      preferred_sinks_patterns=(
        ${preferred_sinks_patterns}
      )

      preferred_sources_patterns=(
        ${preferred_sources_patterns}
      )

      list_sinks() {
        ${pkgs.pulseaudio}/bin/pactl list short sinks | ${pkgs.gawk}/bin/awk '{print $2}'
      }
      find_best_sink() {
        local d
        local s
        for d in "''${bluez_devices[@]}"; do
          s="bluez_output.${d}.1"
          if list_sinks | ${pkgs.gnugrep}/bin/grep -i -q "$s"; then
            echo "$s"
            return 0
          fi
          s="bluez_output.$(echo "$d" | ${pkgs.gnused}/bin/sed "s/:/_/g").1"
          if list_sinks | ${pkgs.gnugrep}/bin/grep -i -q "$s"; then
            echo "$s"
            return 0
          fi
        done
        for s in "''${preferred_sinks_patterns[@]}"; do
          if list_sinks | ${pkgs.gnugrep}/bin/grep -i -q "$s"; then
            echo "$s"
            return 0
          fi
        done
        return 1
      }
      list_sources() {
        ${pkgs.pulseaudio}/bin/pactl list short sources | ${pkgs.gnugrep}/bin/grep -v "monitor$" | ${pkgs.gawk}/bin/awk '{print $2}'
      }
      find_best_source() {
        local d
        local s
        for d in "''${bluez_devices[@]}"; do
          s="bluez_input.${d}"
          if list_sources | ${pkgs.gnugrep}/bin/grep -i -q "$s"; then
            echo "$s"
            return 0
          fi
          s="bluez_input.$(echo "$d" | ${pkgs.gnused}/bin/sed "s/:/_/g")"
          if list_sources | ${pkgs.gnugrep}/bin/grep -i -q "$s"; then
            echo "$s"
            return 0
          fi
        done
        for s in "''${preferred_sources_patterns[@]}"; do
          if list_sources | ${pkgs.gnugrep}/bin/grep -i -q "$s"; then
            echo "$s"
            return 0
          fi
        done
        return 1
      }

      setup_sink() (
          best_sink=$(find_best_sink) || {
              echo "⚠️  None of the preferred sinks are currently available. Found:" >&2
              list_sinks >&2
              return 0
          }
          set -x
          ${pkgs.pulseaudio}/bin/pactl set-default-sink "$best_sink"
          ${pkgs.pulseaudio}/bin/pactl set-sink-volume "$best_sink" 100%
      )

      setup_source() (
          best_source=$(find_best_source) || {
              echo "⚠️  None of the preferred sources are currently available. Found:" >&2
              list_sources >&2
              return 0
          }
          set -x
          ${pkgs.pulseaudio}/bin/pactl set-default-source "$best_source"
          ${pkgs.pulseaudio}/bin/pactl set-source-volume "$best_source" 100%
      )


      setup_sink
      setup_source

      if [[ "$#" -eq 0 || "$1" == "pulsemixer" ]]; then
        ${pkgs.pulsemixer}/bin/pulsemixer
      fi
    '';
in
{
  options.myconfig.desktop.audio.fix-audio = {
    enable = lib.mkEnableOption "myconfig.desktop.audio.fix-audio";

    bluez_devices = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [
        "AC:80:0A:2A:10:6F"
        "EC:66:D1:BD:E4:98"
        "EC:66:D1:B4:C8:3B"
      ];
    };

    preferred_sinks_patterns = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [
        "alsa_output.usb-FiiO_DigiHug_USB_Audio-01.analog-stereo"
        "alsa_output.pci-0000_c1_00.1.hdmi-stereo-extra2"
        "alsa_output.pci-0000_c1_00.6.analog-stereo"
        "usb-FiiO_DigiHug_USB_Audio-01.analog-stereo"
        "hdmi-stereo"
        "alsa_output.pci-0000_c1_00.6.analog-stereo"
      ];
    };

    preferred_sources_patterns = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [
        "alsa_input.usb-BLUE_MICROPHONE_Blue_Snowball_797_2020_07_31_01554-00.mono-fallback"
        "alsa_input.usb-046d_HD_Pro_Webcam_C920_9C1E301F-02.analog-stereo"
      ];
    };

    script = lib.mkOption {
      type = lib.types.path;
      default =
        mkScript config.myconfig.desktop.audio.fix-audio.bluez_devices
          config.myconfig.desktop.audio.fix-audio.preferred_sinks_patterns
          config.myconfig.desktop.audio.fix-audio.preferred_sources_patterns;
    };
  };
  config = lib.mkIf config.myconfig.desktop.audio.fix-audio.enable {

    home-manager.sharedModules = [
      { home.packages = [ config.myconfig.desktop.audio.fix-audio.script ]; }
    ];
  };
}
