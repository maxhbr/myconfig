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
              # --set IDF_PATH "${inputs.esp-idf}" \
              # --set IDF_TOOLS_PATH "${inputs.esp-idf}/tools" \
  };
})
