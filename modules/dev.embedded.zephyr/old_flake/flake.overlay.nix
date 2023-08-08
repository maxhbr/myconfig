{ self, nixpkgs, ... }@inputs:

(final: prev:
  with final;
  let
    # cmsis-pack-manager = final.python3Packages.callPackage ./pyocd/cmsis-pack-manager.nix { };
    # pyocd = final.python3Packages.callPackage ./pyocd { inherit cmsis-pack-manager; };
    # pyocd_pemicro = final.python3Packages.callPackage ./pyocd/pyocd_pemicro.nix { inherit pyocd;};
    jlink = final.callPackage ./jlink { };
    segger-jlink = final.callPackage ./segger-jlink { acceptLicense = true; };
    # nRF-Command-Line-Tools = final.callPackage ./nRF-Command-Line-Tools { };

    # let
    #   pc-ble-overlay = (final: prev: {
    #     pc-ble-driver = let version = "4.1.2";
    #     in prev.pc-ble-driver.overrideAttrs (old: {
    #       inherit version;
    #       src = fetchFromGitHub {
    #         owner = "NordicSemiconductor";
    #         repo = "pc-ble-driver";
    #         rev = "v${version}";
    #         sha256 = "s6SnOLAJ8fwxLQR7PuOLvlaiX61Hhz/MrzQ8h5ApBEQ=";
    #       };
    #       cmakeFlags = [ "-DNRF_BLE_DRIVER_VERSION=${version}" ];
    #       buildInputs = old.buildInputs ++ [ final.spdlog ];
    #     });
    #   });
    #   pkgs2105 = import inputs.rel2105 {
    #     inherit pkgs system;
    #     config = (config // { allowUnfree = true; });
    #     overlays = [ pc-ble-overlay ];
    #   };
    # in pkgs2105.nrfutil;

    python3west = final.python3.withPackages (pp:
      with pp; [
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
        # pyocd
        # gui
        tkinter
        # esp
        future
        cryptography
        setuptools
        pyparsing
        click
        kconfiglib
        # SEGGER
        pylink-square
      ]);
    baseInputs = [
      ninja
      which
      cmake
      dtc
      gperf
      # openocd
      dfu-util
      bossa
      pkgs.nrfutil
      # nRF-Command-Line-Tools
      # jlink
      segger-jlink
      srecord # for srec_cat
    ];
    my-west-fun = { pnameext ? "", moreBuildInputs ? [ ], wrapperArgs ? "" }:
      (stdenv.mkDerivation (rec {
        pname = "my-west" + pnameext;
        version = "1.0";

        buildInputs = moreBuildInputs ++ baseInputs
          ++ (with pkgs; [ git python3west ]);

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
    inherit jlink segger-jlink;
    zephyrenv = { inherit baseInputs; };
    my-west = my-west-fun { };
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

      buildInputs = baseInputs ++ (with pkgs; [ git ]);

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

    my-west-arm = let
      gcc = pkgs.gcc-arm-embedded;
      binutils = pkgs.pkgsCross.arm-embedded.buildPackages.binutils;
      arm-toolchain = pkgs.buildEnv {
        name = "arm-toolchain";
        paths = [ gcc binutils ] ++ baseInputs;
      };
    in my-west-fun {
      pnameext = "-arm";
      moreBuildInputs = [ gcc binutils stdenv.cc.cc.lib ];
      wrapperArgs = ''
        --set ZEPHYR_TOOLCHAIN_VARIANT "gnuarmemb" \
        --set GNUARMEMB_TOOLCHAIN_PATH "${arm-toolchain}"
      '';
    };
    # my-west-riscv = my-west-fun {
    #   pnameext = "-riscv";
    #   wrapperArgs = ''
    #     --set ZEPHYR_TOOLCHAIN_VARIANT zephyr \
    #     --set OPENOCD ${pkgs.openocd}/bin/openocd
    #   '';
    # };
    my-west-esp32 =
      let esp32-toolchain = (pkgs.callPackage ./esp32-toolchain.nix { });
      in my-west-fun {
        pnameext = "-esp32";
        moreBuildInputs = [
          esptool
          esp32-toolchain
          gawk
          gettext
          automake
          bison
          flex
          texinfo
          help2man
          libtool
          autoconf
          ncurses5
          glibcLocales
        ];
        wrapperArgs = ''
          --set NIX_CFLAGS_LINK -lncurses \
          --set ZEPHYR_TOOLCHAIN_VARIANT "espressif" \
          --set ESPRESSIF_TOOLCHAIN_PATH "${esp32-toolchain}"
        '';
      };
  })
