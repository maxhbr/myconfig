{ self, nixpkgs,  ... }@inputs:

(final: prev: with final; let
  python3west = pkgs.python3.withPackages (pp: with pp; [
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
    # gui
    tkinter
    # esp
    future cryptography setuptools pyparsing click
    kconfiglib
  ]);
  baseInputs = [
    ninja
    which
    git
    cmake
    dtc
    gperf
    openocd
    dfu-util
    nrfutil
    bossa
    python3west
  ];
  my-west-fun = {pnameext ? "", moreBuildInputs ? [], wrapperArgs ? ""}: (
    stdenv.mkDerivation (rec {
      pname = "my-west" + pnameext;
      version = "1.0";

      buildInputs = moreBuildInputs ++ baseInputs;

      nativeBuildInputs = [ pkgs.makeWrapper ];

      phases = [ "installPhase" ];

      installPhase = ''
            mkdir -p $out/bin
            makeWrapper ${python3west}/bin/west $out/bin/west${pnameext} \
              --prefix PATH : "${lib.makeBinPath buildInputs}" \
              --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath buildInputs}" \
              --set PYTHONPATH "${python3west}/${python3west.sitePackages}" ${wrapperArgs}
          '';
    }));
in {
  my-west = my-west-fun {};
  my-west-update = writeShellScriptBin "west-update" ''
          cd $HOME/zephyrproject
          ${my-west}/bin/west update
          ${git}/bin/git add bootloader modules tools zephyr
         '';
  my-west-init = writeShellScriptBin "west-init" ''
          ${my-west}/bin/west init $HOME/zephyrproject
          cd $HOME/zephyrproject
          ${git}/bin/git init
          ${git}/bin/git add .
          ${git}/bin/git commit -am "init"
          ${my-west-update}/bin/west-update
          ${git}/bin/git add .
          ${git}/bin/git commit -am "update"
         '';

  my-platformio-zephyr = stdenv.mkDerivation (rec {
      pname = "my-platformio-zephyr";
      version = "1.0";

      buildInputs = baseInputs;

      nativeBuildInputs = [ pkgs.makeWrapper ] ++ (with pkgs; [ libudev ]);

      phases = [ "installPhase" ];

      installPhase = ''
            mkdir -p $out/bin
            makeWrapper ${pkgs.platformio}/bin/platformio $out/bin/platformio-zephyr \
              --prefix PATH : "${lib.makeBinPath buildInputs}" \
              --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath buildInputs}" \
              --set PYTHONPATH "${python3west}/${python3west.sitePackages}"
          '';
    });

  my-west-arm =
    let
      gcc = pkgs.gcc-arm-embedded;
      binutils = pkgs.pkgsCross.arm-embedded.buildPackages.binutils;
      arm-toolchain = pkgs.buildEnv {
        name = "arm-toolchain";
        paths = [ gcc binutils ] ++ baseInputs;
      };
    in my-west-fun {
      pnameext = "-arm";
      moreBuildInputs =
        [
          gcc
          binutils
          stdenv.cc.cc.lib
        ];
      wrapperArgs = ''
              --set ZEPHYR_TOOLCHAIN_VARIANT "gnuarmemb" \
              --set GNUARMEMB_TOOLCHAIN_PATH "${arm-toolchain}"
        '';
    };
  my-west-riscv = my-west-fun {
    pnameext = "-riscv";
    moreBuildInputs =
      [ openocd
        # git cmake ninja-build gperf\ccache dfu-util device-tree-compiler wget python3-pip python3-setuptools\python3-wheel xz-utils file make gcc gcc-multilib
      ];
    wrapperArgs = ''
              --set ZEPHYR_TOOLCHAIN_VARIANT zephyr \
              --set OPENOCD ${pkgs.openocd}/bin/openocd
        '';
  };
  my-west-esp32 = let
    esp32-toolchain = (pkgs.callPackage ./esp32-toolchain.nix {});
  in my-west-fun {
    pnameext = "-esp32";
    moreBuildInputs =
      [
        esptool
        esp32-toolchain
        gawk gettext automake bison flex texinfo help2man libtool autoconf ncurses5 glibcLocales
      ];
    wrapperArgs = ''
              --set NIX_CFLAGS_LINK -lncurses \
              --set ZEPHYR_TOOLCHAIN_VARIANT "espressif" \
              --set ESPRESSIF_TOOLCHAIN_PATH "${esp32-toolchain}"
        '';
  };
  my-minicom-esp32 = with pkgs; writeShellScriptBin "minicom-esp32" ''
    minicom -con -b 115200 -D ''${1:-/dev/ttyUSB0}
  '';
})
