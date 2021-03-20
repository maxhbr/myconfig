{
  description = "my project description";

  inputs = {
    platformio-core.url = "github:platformio/platformio-core";
    platformio-core.flake = false;
  };

  outputs = { self, nixpkgs,  ... }@inputs:
    let
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
      systems = [ "x86_64-linux" ];
    in
    {
      nixosModule = { config, lib, pkgs, ... }: let
        platformio-udev-rules = pkgs.writeTextFile {
          name = "platformio-udev-rules";
          text = builtins.readFile "${inputs.platformio-core}/scripts/99-platformio-udev.rules";
          destination = "/etc/udev/rules.d/99-platformio.rules";
        };
      in {
        nixpkgs.overlays = [ self.overlay ];
        home-manager.sharedModules = [{
          home.packages = with pkgs; [
            platformio openocd

            # devtools
            ninja
            which
            git
            cmake
            dtc
            gperf
            openocd
            dfu-util
            bossa
            python3
          ] ++ (with python3Packages; [
            west
            docutils
            wheel
            breathe
            sphinx
            sphinx_rtd_theme
            pyyaml
            ply
            pyelftools
            pyserial
            pykwalify
            colorama
            pillow
            intelhex
            pytest
            gcovr
          ]);
        }];
        services.udev.packages = [ platformio-udev-rules pkgs.openocd ];
      };
      overlay = final: prev: {
        my-west = with final; stdenv.mkDerivation (
        let
          inputs = );
          gcc = pkgs.gcc-arm-embedded;
          binutils = pkgs.pkgsCross.arm-embedded.buildPackages.binutils;
          toolchain = pkgs.buildEnv {
            name = "arm-toolchain";
            paths = [ gcc binutils ] ++ inputs;
          };
        in rec {
          pname = "my-west";
          version = "1.0";

          buildInputs =
            [ stdenv.cc.cc.lib
            ];

          nativeBuildInputs = [ pkgs.makeWrapper ];

          phases = [ "installPhase" ];

          installPhase = ''
            mkdir -p $out/bin
            makeWrapper ${toolchain}/bin/west $out/bin/mywest \
              --prefix PATH : "${lib.makeBinPath buildInputs}" \
              --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath buildInputs}" \
              --set ZEPHYR_TOOLCHAIN_VARIANT "gnuarmemb" \
              --set GNUARMEMB_TOOLCHAIN_PATH "${toolchain}" \
              --set ESPRESSIF_TOOLCHAIN_PATH "~/.espressif/tools/xtensa-esp32-elf/esp-2020r3-8.4.0/xtensa-esp32-elf"
          '';
        });
      };

      packages = forAllSystems (system: {
        my-west = (import nixpkgs { inherit system; overlays = [ self.overlay ]; }).my-west;
      });

      defaultPackage = forAllSystems (system: self.packages.${system}.my-west);

      defaultApp = forAllSystems (system: {
        type = "app";
        program = "${self.defaultPackage."${system}"}/bin/mywest";
      });
    };
}
