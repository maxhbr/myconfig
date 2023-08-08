{ config, lib, pkgs, ... }:
let
  cfg = config.myconfig;
in {
  options.myconfig = with lib; {
    dev.embedded.enable = mkEnableOption "embedded";
  };
  config = (lib.mkIf cfg.dev.embedded.enable {
    nixpkgs.config.allowUnfree = true;
    nixpkgs.config.segger-jlink.acceptLicense = true;
    home-manager.sharedModules = [{
      home.packages = with pkgs; [
        # openocd
        picocom
        minicom
        nrfutil
        (writeShellScriptBin "flash-nrf52840dongle" ''
          set -euo pipefail
          in=build/zephyr/zephyr.hex
          out=build/zephyr.zip
          if [[ -f "$in" ]]; then
            set -x
            ${pkgs.nrfutil}/bin/nrfutil pkg generate --hw-version 52 --sd-req=0x00 \
                    --application "$in" \
                    --application-version 1 "$out"
            ${pkgs.nrfutil}/bin/nrfutil dfu usb-serial -pkg "$out" -p "''${1:-/dev/ttyACM0}"
          else
            echo "\$in=$in not found"
          fi
        '')
        (writeShellScriptBin "clang-format" ''
          exec ${llvmPackages.clang-unwrapped}/bin/clang-format "$@"
        '')
        # teensy
        teensy-loader-cli
        tytools
        # # mbed
        # mbed-cli
      ];
    }];
    services.udev.packages = let
      # platformio-udev-rules = pkgs.writeTextFile {
      #   name = "platformio-udev-rules";
      #   text = builtins.readFile
      #     "${inputs.platformio-core}/platformio/assets/system/99-platformio-udev.rules";
      #   destination = "/etc/udev/rules.d/99-platformio.rules";
      # };
      segger-modemmanager-blacklist-udev-rules = pkgs.writeTextFile {
        name = "segger-modemmanager-blacklist-udev-rules";
        # https://docs.zephyrproject.org/2.5.0/guides/tools/nordic_segger.html#gnu-linux
        text = ''ATTRS{idVendor}=="1366", ENV{ID_MM_DEVICE_IGNORE}="1"'';
        destination = "/etc/udev/rules.d/99-segger-modemmanager-blacklist.rules";
      };
    in with pkgs; [
      # platformio-udev-rules
      # pkgs.platformio
      segger-modemmanager-blacklist-udev-rules
      # openocd
      segger-jlink
      stlink
      teensy-udev-rules
      picoprobe-udev-rules
    ];
  });
}
