# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# The `embedded` mysbx profile for p14: a `mysbx-embedded` launcher
# with firmware-development tools on the sandbox PATH, for agents that
# build and inspect Zephyr/STM32/RP2040-class projects (see
# ../../modules/dev/dev.embedded.nix for the host-side user setup).
#
# Build and emulation only: mysbx's bwrap backend creates a private
# `/dev` (bwrap.rs `base_binds`), so no serial port, USB probe or
# hidraw node is reachable inside the sandbox — flashing real hardware
# stays with the `agnt-embedded` agent user
# (./myconfig.agentUsers.nix), whose groups give it /dev/ttyACM* and
# probe access. What this profile adds is the toolchain: cross
# compilers, debug-probe clients (usable against a networked probe or
# QEMU), serial-terminal emulators for logs captured elsewhere, and
# qemu-system-arm for board emulation.
{
  pkgs,
  ...
}:
{
  config = {
    myconfig.ai.dev.mysbx.profiles.embedded.extraTools =
      # Cross toolchains: ARM Cortex-M (gcc-arm-embedded is a full
      # xpack-style distribution with newlib) — the default target set
      # covers arm-none-eabi. picotool needs the RP2040 SDK anyway.
      with pkgs; [
        gcc-arm-embedded

        # Debug-probe clients: openocd (SWD/JTAG, also drives QEMU),
        # probe-rs-tools (cargo-flash/cargo-embed CLI suite), pyocd
        # (python, CMSIS-Pack based), stlink (st-flash/st-info) and
        # picotool (RP2040/RP2350).
        openocd
        probe-rs-tools
        pyocd
        stlink
        picotool

        # Flashing over bootloader protocols: dfu-util (DFU),
        # stm32flash (UART bootloader), teensy-loader-cli, and
        # platformio-core for Arduino/framework projects.
        dfu-util
        stm32flash
        teensy-loader-cli
        platformio-core

        # Zephyr tooling: west is the meta-tool (init/update/flash);
        # the Zephyr SDK itself stays outside the closure — a project
        # pins its own SDK version via west, and builds fetch it into
        # the sandbox's writable tmpfs home. toPythonApplication gives
        # a standalone `west` binary that coexists with the baseline
        # python3 in toolsEnv (a python3.withPackages env would
        # collide with it in buildEnv's bin/).
        (python3Packages.toPythonApplication python3Packages.west)

        # Board emulation: qemu carries all softmmu targets including
        # arm (qemu-system-arm) — mps2-an385, netduino and the STM32
        # Discovery boards Zephyr/QEMU support run entirely inside.
        qemu

        # Serial terminals: tio for captured UART logs (e.g. from a
        # usbip-exported device or a socket), picocom/minicom as the
        # classic alternatives.
        tio
        picocom
        minicom

        # Binutils for artifact inspection: objdump/readelf/size on
        # the ELF files the toolchains emit (`arm-none-eabi-*` are in
        # gcc-arm-embedded itself; plain objdump covers host builds).
        binutils
      ];
  };
}
