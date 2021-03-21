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
  in {
  my-west = stdenv.mkDerivation (
    let
      gcc = pkgs.gcc-arm-embedded;
      binutils = pkgs.pkgsCross.arm-embedded.buildPackages.binutils;
      toolchain = pkgs.buildEnv {
        name = "arm-toolchain";
        paths = [ gcc binutils ] ++ baseInputs;
      };
    in rec {
      pname = "my-west-gnuarmemb";
      version = "1.0";

      buildInputs =
        [
          gcc
          binutils
          stdenv.cc.cc.lib
        ] ++ baseInputs;

      nativeBuildInputs = [ pkgs.makeWrapper ];

      phases = [ "installPhase" ];

      installPhase = ''
            mkdir -p $out/bin
            makeWrapper ${toolchain}/bin/west $out/bin/west \
              --prefix PATH : "${lib.makeBinPath buildInputs}" \
              --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath buildInputs}" \
              --set PYTHONPATH "${python3west}/${python3west.sitePackages}" \
              --set ZEPHYR_TOOLCHAIN_VARIANT "gnuarmemb" \
              --set GNUARMEMB_TOOLCHAIN_PATH "${toolchain}"
          '';
    });
      # moreBuildInputs = [
      #   #esp:
      #   esptool
      #   (pkgs.callPackage ./esp32-toolchain.nix {})
      #   # gawk gperf gettext automake bison flex texinfo help2man libtool autoconf ncurses5 cmake glibcLocales
      #   # (python2.withPackages (ppkgs: with ppkgs; [ pyserial future cryptography setuptools pyelftools pyparsing click ]))
      # ];
            # makeWrapper ${toolchain}/bin/west $out/bin/west-esp \
            #   --set NIX_CFLAGS_LINK -lncurses \
            #   --set IDF_PATH "${inputs.esp-idf}" \
            #   --set IDF_TOOLS_PATH "${inputs.esp-idf}/tools" \
            #   --prefix PATH : "${inputs.esp-idf}/tools" \
            #   --prefix PATH : "${lib.makeBinPath buildInputs}" \
            #   --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath buildInputs}" \
            #   --set PYTHONPATH "${python3west}/${python3west.sitePackages}" \
            #   --set ZEPHYR_TOOLCHAIN_VARIANT "espressif"
  my-west-update = writeShellScriptBin "mywest-update" ''
          cd $HOME/zephyrproject
          ${my-west}/bin/west update
         '';
  my-west-init = writeShellScriptBin "mywest-init" ''
          ${my-west}/bin/west init $HOME/zephyrproject
          cd $HOME/zephyrproject
          ${git}/bin/git init
          ${git}/bin/git add .
          ${git}/bin/git commit -am "init"
          ${my-west-update}/bin/west-update
          ${git}/bin/git add .
          ${git}/bin/git commit -am "update"
         '';
})
