#!/usr/bin/env bash

set -euo pipefail

bluez_devices=(
  "AC:80:0A:2A:10:6F"
  "EC:66:D1:BD:E4:98"
  "EC:66:D1:B4:C8:3B"
)

preferred_sinks_patterns=(
  "alsa_output.usb-FiiO_DigiHug_USB_Audio-01.analog-stereo"
  "alsa_output.pci-0000_c1_00.1.hdmi-stereo-extra2"
  "alsa_output.pci-0000_c1_00.6.analog-stereo"
  "usb-FiiO_DigiHug_USB_Audio-01.analog-stereo"
  "hdmi-stereo"
  "alsa_output.pci-0000_c1_00.6.analog-stereo"
)

preferred_sources_patterns=(
  "alsa_input.usb-BLUE_MICROPHONE_Blue_Snowball_797_2020_07_31_01554-00.mono-fallback"
  "alsa_input.usb-046d_HD_Pro_Webcam_C920_9C1E301F-02.analog-stereo"
  "usb-BLUE_MICROPHONE_Blue_Snowball_797_2020_07_31_01554-00.mono-fallback"
  "usb-046d_HD_Pro_Webcam_C920_9C1E301F-02.analog-stereo"
  "alsa_input.pci-0000_c1_00.6.analog-stereo"
)

list_sinks() {
  pactl list short sinks | awk '{print $2}'
}
find_best_sink() {
  local d
  local s
  for d in "${bluez_devices[@]}"; do
    s="bluez_output.${d}.1"
    if list_sinks | grep -i -q "$s"; then
      echo "$s"
      return 0
    fi
    s="bluez_output.$(echo "$d" | sed "s/:/_/g").1"
    if list_sinks | grep -i -q "$s"; then
      echo "$s"
      return 0
    fi
  done
  for s in "${preferred_sinks_patterns[@]}"; do
    if list_sinks | grep -i -q "$s"; then
      echo "$s"
      return 0
    fi
  done
  return 1
}
list_sources() {
  pactl list short sources | grep -v "monitor$" | awk '{print $2}'
}
find_best_source() {
  local d
  local s
  for d in "${bluez_devices[@]}"; do
    s="bluez_input.${d}"
    if list_sources | grep -i -q "$s"; then
      echo "$s"
      return 0
    fi
    s="bluez_input.$(echo "$d" | sed "s/:/_/g")"
    if list_sources | grep -i -q "$s"; then
      echo "$s"
      return 0
    fi
  done
  for s in "${preferred_sources_patterns[@]}"; do
    if list_sources | grep -i -q "$s"; then
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
    pactl set-default-sink "$best_sink"
    pactl set-sink-volume "$best_sink" 100%
)

setup_source() (
    best_source=$(find_best_source) || {
        echo "⚠️  None of the preferred sources are currently available. Found:" >&2
        list_sources >&2
        return 0
    }
    set -x
    pactl set-default-source "$best_source"
    pactl set-source-volume "$best_source" 100%
)


setup_sink
setup_source
pulsemixer
