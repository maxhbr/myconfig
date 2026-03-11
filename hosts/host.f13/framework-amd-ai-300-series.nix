# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
{
  # TODO:

  # Instead of this file one should just import  "inputs.nixos-hardware.nixosModules.framework-amd-ai-300-series" directly
  # But it fails in a recursion mess semoewhere.

  imports = [
    ####################################################################################################################
    # copied from https://github.com/NixOS/nixos-hardware/blob/master/framework/13-inch/amd-ai-300-series/default.nix
    {
      services.fwupd.enable = true;

      hardware.framework.laptop13.audioEnhancement.rawDeviceName =
        lib.mkDefault "alsa_output.pci-0000_c1_00.6.analog-stereo";

      # NOTE: amdgpu.dcdebugmask=0x10, rtc_cmos.use_acpi_alarm=1, amd_pstate,
      # and power-profiles-daemon are already set by the amd.nix import below.
      # Only add params here that are NOT in upstream nixos-hardware.
      boot.kernelParams = [
        # Enable AMD PMC Smart Trace Buffer for s2idle debugging and
        # to help the PMC driver initialize properly for S0ix entry
        "amd_pmc.enable_stb=1"
      ];

    }
    # ####################################################################################################################
    # copied from https://github.com/NixOS/nixos-hardware/blob/master/framework/13-inch/common/default.nix
    "${inputs.nixos-hardware}/framework/13-inch/common/../../../common/pc/laptop"
    "${inputs.nixos-hardware}/framework/13-inch/common/../../../common/pc/ssd"
    "${inputs.nixos-hardware}/framework/13-inch/common/../../bluetooth.nix"
    "${inputs.nixos-hardware}/framework/13-inch/common/../../framework-tool.nix"
    "${inputs.nixos-hardware}/framework/13-inch/common/./audio.nix"
    {
      # Fix TRRS headphones missing a mic
      # https://community.frame.work/t/headset-microphone-on-linux/12387/3
      boot.extraModprobeConfig = lib.mkIf (lib.versionOlder config.boot.kernelPackages.kernel.version "6.6.8") ''
        options snd-hda-intel model=dell-headset-multi
      '';

      # For fingerprint support
      services.fprintd.enable = lib.mkDefault true;

      # Custom udev rules
      services.udev.extraRules = ''
        # Ethernet expansion card support
        ACTION=="add", SUBSYSTEM=="usb", ATTR{idVendor}=="0bda", ATTR{idProduct}=="8156", ATTR{power/autosuspend}="20"
      '';

      # Fix font sizes in X
      # services.xserver.dpi = 200;

      # Needed for desktop environments to detect/manage display brightness
      hardware.sensor.iio.enable = lib.mkDefault true;
    }
    # ####################################################################################################################
    # copied and modified from https://github.com/NixOS/nixos-hardware/blob/master/framework/kmod.nix
    # {
    #   config.boot = {
    #     extraModulePackages = with config.boot.kernelPackages; [
    #       framework-laptop-kmod
    #     ];

    #     # https://github.com/DHowett/framework-laptop-kmod?tab=readme-ov-file#usage
    #     kernelModules = [
    #       "cros_ec"
    #       "cros_ec_lpcs"
    #     ];

    #     # add required patch if enabled on kernel <6.10
    #     kernelPatches = [
    #       rec {
    #         name = "platform/chrome: cros_ec_lpc: add support for AMD Framework Laptops";
    #         msgid = "20240403004713.130365-1-dustin@howett.net";
    #         version = "3";
    #         hash = "sha256-aQSyys8CMzlj9EdNhg8vtp76fg1qEwUVeJL0E+8w5HU=";
    #         patch =
    #           pkgs.runCommandLocal "patch-${msgid}"
    #             {
    #               nativeBuildInputs = with pkgs; [
    #                 b4
    #                 git
    #                 cacert
    #               ];
    #               SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";

    #               outputHash = hash;
    #             }
    #             ''
    #               export HOME="$TMP"
    #               PYTHONHASHSEED=0 ${pkgs.b4}/bin/b4 -n am -C -T -v ${version} -o- "${msgid}" > "$out"
    #             '';
    #       }
    #     ];
    #   };
    # }
    "${inputs.nixos-hardware}/framework/13-inch/common/amd.nix"
  ];
}
