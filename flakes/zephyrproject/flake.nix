{
  description = "my project description";

  inputs = {
    platformio-core.url = "github:platformio/platformio-core";
    rel2105.url = "github:nixos/nixpkgs/release-21.05";
    platformio-core.flake = false;
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
      systems = [ "x86_64-linux" ];

      allpackages = pkgs:
        with pkgs;
        zephyrenv.baseInputs ++ [
          nrfutil
          nRF-Command-Line-Tools
          segger-jlink
          my-west
          my-west-arm
          my-west-riscv
          my-west-esp32
          my-west-update
          my-west-init
        ];
    in {
      nixosModule = { config, lib, pkgs, ... }:
        let
          platformio-udev-rules = pkgs.writeTextFile {
            name = "platformio-udev-rules";
            text = builtins.readFile
              "${inputs.platformio-core}/platformio/assets/system/99-platformio-udev.rules";
            destination = "/etc/udev/rules.d/99-platformio.rules";
          };
          segger-modemmanager-blacklist-udev-rules = pkgs.writeTextFile {
            name = "segger-modemmanager-blacklist-udev-rules";
            # https://docs.zephyrproject.org/2.5.0/guides/tools/nordic_segger.html#gnu-linux
            text = ''ATTRS{idVendor}=="1366", ENV{ID_MM_DEVICE_IGNORE}="1"'';
            destination =
              "/etc/udev/rules.d/99-segger-modemmanager-blacklist.rules";
          };
        in {
          nixpkgs.overlays = [ self.overlay ];
          home-manager.sharedModules = [{
            home.packages = (allpackages pkgs) ++ (with pkgs; [
              openocd
              picocom minicom
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
            ]);
            # home.sessionVariables = {
            #   ZEPHYR_BASE = "/home/mhuber/zephyrproject/zephyr";
            #   IDF_PATH = "/home/mhuber/zephyrproject/modules/hal/espressif";
            #   IDF_TOOLS_PATH = "/home/mhuber/zephyrproject/modules/hal/espressif/tools";
            # };
            programs.fish = {
              shellAbbrs = lib.mapAttrs'
                (name: value: lib.nameValuePair ("west-" + name) value)
                (lib.genAttrs [
                  "adafruit_feather_nrf52840"
                  "adafruit_feather_stm32f405"
                  "b_l4s5i_iot01a"
                  "bbc_microbit_v2"
                  # "frdm_k64f"
                  "lpcxpresso55s69_cpu0"
                  # "mimxrt1060_evk"
                  "nrf52840dongle_nrf52840"
                  "nrf5340dk_nrf5340_cpuapp"
                  "nucleo_f767zi"
                  "nucleo_h745zi_q_m4"
                  "nucleo_h745zi_q_m7"
                  "nucleo_l432kc"
                  "nucleo_wb55rg"
                  "teensy40"
                  "teensy41"
                  "xiao_ble"
                ] (board: "west-arm build -p always -b ${board} ."));
            };
          }];
          services.udev.packages = [
            platformio-udev-rules
            pkgs.platformio
            segger-modemmanager-blacklist-udev-rules
            pkgs.openocd
            pkgs.segger-jlink
            pkgs.stlink
            pkgs.teensy-udev-rules
          ];
        };

      overlay = import ./flake.overlay.nix inputs;

      packages = forAllSystems (system: {
        my-west = (import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        }).my-west;
        my-west-arm = (import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        }).my-west-arm;
        my-west-riscv = (import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        }).my-west-riscv;
        my-west-esp32 = (import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        }).my-west-esp32;
        my-west-init = (import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        }).my-west-init;
        my-west-update = (import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        }).my-west-update;
      });

      defaultPackage =
        forAllSystems (system: self.packages.${system}.my-west-arm);

      defaultApp = forAllSystems (system: {
        type = "app";
        program = "${self.defaultPackage."${system}"}/bin/mywest";
      });

      devShell = forAllSystems (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ self.overlay ];
            config.allowUnfree = true;
          };
        in pkgs.mkShell { nativeBuildInputs = allpackages pkgs; });
    };
}
