# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  config,
  lib,
  myconfig,
  ...
}:
let
  cfg = config.myconfig.dev.embedded;
  user = myconfig.user;

  # One list, two consumers: the `mysbx-embedded` sandbox profile
  # (extraTools below) and the host-side home.packages — the same
  # firmware-dev toolset inside and outside the sandbox.
  #
  # Build and emulation only inside the sandbox: mysbx's bwrap backend
  # creates a private `/dev`, so no serial port, USB probe or hidraw
  # node is reachable there — the probe clients in this list run
  # against a networked probe or QEMU. Flashing real hardware stays
  # with the `agnt-embedded` agent user (hosts/host.*/
  # myconfig.agentUsers.nix), whose groups give it /dev/ttyACM* and
  # probe access.
  #
  # Cross toolchains: ARM Cortex-M (gcc-arm-embedded is a full
  # xpack-style distribution with newlib) — the default target set
  # covers arm-none-eabi. picotool needs the RP2040 SDK anyway.
  tools = with pkgs; [
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

    gdb
  ];

  # Binutils for artifact inspection: objdump/readelf/size on the ELF
  # files the toolchains emit (`arm-none-eabi-*` are in gcc-arm-embedded
  # itself; plain objdump covers host builds). The `embedded-dev-env`
  # session prepends its packages to PATH, and a GNU objdump would
  # shadow dev.rust's LLVM bintools there, so hosts with dev.rust
  # enabled keep the LLVM wrapper instead. The mysbx profile never
  # pulls dev.rust's bintools, so it always keeps GNU binutils.
  binutilsHost = lib.optionals (!config.myconfig.dev.rust.enable) [
    pkgs.binutils
  ];
  binutilsSandbox = [ pkgs.binutils ];
in
{
  config = lib.mkIf cfg.enable {
    myconfig.ai.dev.mysbx.profiles.embedded.extraTools = tools ++ binutilsSandbox;
    myconfig.dev.envs.embedded.packages = tools ++ binutilsHost;
    users.groups.plugdev = { };
    users.users."${user}".extraGroups = lib.mkAfter [
      "dialout"
      "uucp"
      "plugdev"
    ];
    nixpkgs.config = {
      allowUnfreePredicate =
        pkg:
        builtins.elem (lib.getName pkg) [
          "segger-jlink"
          "STM32CubeProg"
        ];
      segger-jlink.acceptLicense = true;
    };
    services.udev.packages = with pkgs; [
      platformio-core
      openocd
      stlink
      dfu-util
      teensy-udev-rules
      # JLink_Linux_V970_x86_64.tgz is currently unfetchable (segger.com
      # rejects resume, not in the tarball mirror); re-enable once it builds.
      # segger-jlink-headless
    ];
    # Extra “belt-and-suspenders” rules for very common probes:
    services.udev.extraRules = ''
      # CMSIS-DAP (hidraw access)
      KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="0d28", TAG+="uaccess"

      # STMicroelectronics ST-LINK/V2 (various IDs; keep MODE conservative)
      SUBSYSTEM=="usb", ATTR{idVendor}=="0483", ATTR{idProduct}=="3748", TAG+="uaccess"
      SUBSYSTEM=="usb", ATTR{idVendor}=="0483", ATTR{idProduct}=="374b", TAG+="uaccess"
    '';
  };
}
