# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:
{
  imports = [
    ./dev.nix
  ];
  config = {
    environment.systemPackages = with pkgs.unstable; [
      platformio
    ];

    nixpkgs.config.packageOverrides = pkgs: {
      platformio-udev-rules = pkgs.writeTextFile {
        name = "platformio-udev-rules";
        # see: https://docs.platformio.org/en/latest/faq.html#platformio-udev-rules
        #   see: https://raw.githubusercontent.com/platformio/platformio-core/master/scripts/99-platformio-udev.rules
        text = ''
# Copyright (c) 2014-present PlatformIO <contact@platformio.org>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#####################################################################################
#
# INSTALLATION
#
# Please visit > https://docs.platformio.org/en/latest/faq.html#platformio-udev-rules
#
#####################################################################################

#
# Boards
#

# CP210X USB UART
SUBSYSTEMS=="usb", ATTRS{idVendor}=="10c4", ATTRS{idProduct}=="ea60", MODE:="0666"

# FT232R USB UART
SUBSYSTEMS=="usb", ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6001", MODE:="0666"

# FT231XS USB UART
SUBSYSTEMS=="usb", ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6015", MODE:="0666"

# Prolific Technology, Inc. PL2303 Serial Port
SUBSYSTEMS=="usb", ATTRS{idVendor}=="067b", ATTRS{idProduct}=="2303", MODE:="0666"

# QinHeng Electronics HL-340 USB-Serial adapter
SUBSYSTEMS=="usb", ATTRS{idVendor}=="1a86", ATTRS{idProduct}=="7523", MODE:="0666"

# Arduino boards
SUBSYSTEMS=="usb", ATTRS{idVendor}=="2341", ATTRS{idProduct}=="[08][02]*", MODE:="0666", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_PORT_IGNORE}="1"
SUBSYSTEMS=="usb", ATTRS{idVendor}=="2a03", ATTRS{idProduct}=="[08][02]*", MODE:="0666", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_PORT_IGNORE}="1"

# Arduino SAM-BA
ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="6124", ENV{ID_MM_DEVICE_IGNORE}="1"
ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="6124", ENV{MTP_NO_PROBE}="1"
SUBSYSTEMS=="usb", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="6124", MODE:="0666"
KERNEL=="ttyACM*", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="6124", MODE:="0666"

# Digistump boards
SUBSYSTEMS=="usb", ATTRS{idVendor}=="16d0", ATTRS{idProduct}=="0753", MODE:="0666"
KERNEL=="ttyACM*", ATTRS{idVendor}=="16d0", ATTRS{idProduct}=="0753", MODE:="0666", ENV{ID_MM_DEVICE_IGNORE}="1"

# STM32 discovery boards, with onboard st/linkv2
SUBSYSTEMS=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="374?", MODE:="0666"

# Maple with DFU
SUBSYSTEMS=="usb", ATTRS{idVendor}=="1eaf", ATTRS{idProduct}=="000[34]", MODE:="0666"

# USBtiny
SUBSYSTEMS=="usb", ATTRS{idProduct}=="0c9f", ATTRS{idVendor}=="1781", MODE="0666"

# USBasp V2.0
SUBSYSTEMS=="usb", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="05dc", MODE:="0666"

# Teensy boards
ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_PORT_IGNORE}="1"
ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789A]?", ENV{MTP_NO_PROBE}="1"
SUBSYSTEMS=="usb", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789ABCD]?", MODE:="0666"
KERNEL=="ttyACM*", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", MODE:="0666"

#TI Stellaris Launchpad
SUBSYSTEMS=="usb", ATTRS{idVendor}=="1cbe", ATTRS{idProduct}=="00fd", MODE="0666"

#TI MSP430 Launchpad
SUBSYSTEMS=="usb", ATTRS{idVendor}=="0451", ATTRS{idProduct}=="f432", MODE="0666"


#
# Debuggers
#

# Black Magic Probe
SUBSYSTEM=="tty", ATTRS{interface}=="Black Magic GDB Server"
SUBSYSTEM=="tty", ATTRS{interface}=="Black Magic UART Port"

# opendous and estick
ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="204f", MODE="0666"

# Original FT232/FT245 VID:PID
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6001", MODE="0666"

# Original FT2232 VID:PID
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6010", MODE="0666"

# Original FT4232 VID:PID
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6011", MODE="0666"

# Original FT232H VID:PID
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6014", MODE="0666"

# DISTORTEC JTAG-lock-pick Tiny 2
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="8220", MODE="0666"

# TUMPA, TUMPA Lite
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="8a98", MODE="0666"
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="8a99", MODE="0666"

# XDS100v2
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="a6d0", MODE="0666"

# Xverve Signalyzer Tool (DT-USB-ST), Signalyzer LITE (DT-USB-SLITE)
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="bca0", MODE="0666"
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="bca1", MODE="0666"

# TI/Luminary Stellaris Evaluation Board FTDI (several)
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="bcd9", MODE="0666"

# TI/Luminary Stellaris In-Circuit Debug Interface FTDI (ICDI) Board
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="bcda", MODE="0666"

# egnite Turtelizer 2
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="bdc8", MODE="0666"

# Section5 ICEbear
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="c140", MODE="0666"
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="c141", MODE="0666"

# Amontec JTAGkey and JTAGkey-tiny
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="cff8", MODE="0666"

# TI ICDI
ATTRS{idVendor}=="0451", ATTRS{idProduct}=="c32a", MODE="0666"

# STLink v1
ATTRS{idVendor}=="0483", ATTRS{idProduct}=="3744", MODE="0666"

# STLink v2
ATTRS{idVendor}=="0483", ATTRS{idProduct}=="3748", MODE="0666"

# STLink v2-1
ATTRS{idVendor}=="0483", ATTRS{idProduct}=="374b", MODE="0666"

# Hilscher NXHX Boards
ATTRS{idVendor}=="0640", ATTRS{idProduct}=="0028", MODE="0666"

# Hitex STR9-comStick
ATTRS{idVendor}=="0640", ATTRS{idProduct}=="002c", MODE="0666"

# Hitex STM32-PerformanceStick
ATTRS{idVendor}=="0640", ATTRS{idProduct}=="002d", MODE="0666"

# Altera USB Blaster
ATTRS{idVendor}=="09fb", ATTRS{idProduct}=="6001", MODE="0666"

# Amontec JTAGkey-HiSpeed
ATTRS{idVendor}=="0fbb", ATTRS{idProduct}=="1000", MODE="0666"

# SEGGER J-Link
ATTRS{idVendor}=="1366", ATTRS{idProduct}=="0101", MODE="0666"
ATTRS{idVendor}=="1366", ATTRS{idProduct}=="0102", MODE="0666"
ATTRS{idVendor}=="1366", ATTRS{idProduct}=="0103", MODE="0666"
ATTRS{idVendor}=="1366", ATTRS{idProduct}=="0104", MODE="0666"
ATTRS{idVendor}=="1366", ATTRS{idProduct}=="0105", MODE="0666"
ATTRS{idVendor}=="1366", ATTRS{idProduct}=="0107", MODE="0666"
ATTRS{idVendor}=="1366", ATTRS{idProduct}=="0108", MODE="0666"
ATTRS{idVendor}=="1366", ATTRS{idProduct}=="1010", MODE="0666"
ATTRS{idVendor}=="1366", ATTRS{idProduct}=="1011", MODE="0666"
ATTRS{idVendor}=="1366", ATTRS{idProduct}=="1012", MODE="0666"
ATTRS{idVendor}=="1366", ATTRS{idProduct}=="1013", MODE="0666"
ATTRS{idVendor}=="1366", ATTRS{idProduct}=="1014", MODE="0666"
ATTRS{idVendor}=="1366", ATTRS{idProduct}=="1015", MODE="0666"
ATTRS{idVendor}=="1366", ATTRS{idProduct}=="1016", MODE="0666"
ATTRS{idVendor}=="1366", ATTRS{idProduct}=="1017", MODE="0666"
ATTRS{idVendor}=="1366", ATTRS{idProduct}=="1018", MODE="0666"

# Raisonance RLink
ATTRS{idVendor}=="138e", ATTRS{idProduct}=="9000", MODE="0666"

# Debug Board for Neo1973
ATTRS{idVendor}=="1457", ATTRS{idProduct}=="5118", MODE="0666"

# Olimex ARM-USB-OCD
ATTRS{idVendor}=="15ba", ATTRS{idProduct}=="0003", MODE="0666"

# Olimex ARM-USB-OCD-TINY
ATTRS{idVendor}=="15ba", ATTRS{idProduct}=="0004", MODE="0666"

# Olimex ARM-JTAG-EW
ATTRS{idVendor}=="15ba", ATTRS{idProduct}=="001e", MODE="0666"

# Olimex ARM-USB-OCD-TINY-H
ATTRS{idVendor}=="15ba", ATTRS{idProduct}=="002a", MODE="0666"

# Olimex ARM-USB-OCD-H
ATTRS{idVendor}=="15ba", ATTRS{idProduct}=="002b", MODE="0666"

# USBprog with OpenOCD firmware
ATTRS{idVendor}=="1781", ATTRS{idProduct}=="0c63", MODE="0666"

# TI/Luminary Stellaris In-Circuit Debug Interface (ICDI) Board
ATTRS{idVendor}=="1cbe", ATTRS{idProduct}=="00fd", MODE="0666"

# Marvell Sheevaplug
ATTRS{idVendor}=="9e88", ATTRS{idProduct}=="9e8f", MODE="0666"

# Keil Software, Inc. ULink
ATTRS{idVendor}=="c251", ATTRS{idProduct}=="2710", MODE="0666"

# CMSIS-DAP compatible adapters
ATTRS{product}=="*CMSIS-DAP*", MODE="0666"

#SEGGER J-LIK
ATTR{idProduct}=="1001", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1002", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1003", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1004", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1005", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1006", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1007", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1008", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1009", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="100a", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="100b", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="100c", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="100d", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="100e", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="100f", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1010", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1011", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1012", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1013", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1014", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1015", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1016", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1017", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1018", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1019", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="101a", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="101b", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="101c", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="101d", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="101e", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="101f", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1020", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1021", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1022", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1023", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1024", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1025", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1026", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1027", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1028", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="1029", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="102a", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="102b", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="102c", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="102d", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="102e", ATTR{idVendor}=="1366", MODE="0666"
ATTR{idProduct}=="102f", ATTR{idVendor}=="1366", MODE="0666"
        '';
        destination = "/etc/udev/rules.d/99-platformio.rules";
      };
    };
    services.udev.packages = [ pkgs.platformio-udev-rules ];
  };
}
