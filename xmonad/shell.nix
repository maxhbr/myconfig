{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, stdenv, xmonad, xmonad-contrib }:
      mkDerivation {
        pname = "myxmonad";
        version = "0.0.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base xmonad xmonad-contrib
                                  nixpkgs.gmp
                                  nixpkgs.xorg.libXinerama
                                  nixpkgs.xorg.libXext
                                  nixpkgs.xorg.libX11
                                  nixpkgs.xorg.libXrandr ];
        executableHaskellDepends = [ base xmonad xmonad-contrib ];
        description = "my xmonad configuration";
        license = stdenv.lib.licenses.mit;
      };
  # LD_LIBRARY_PATH = "${nixpkgs.xorg.libXinerama}/lib:${nixpkgs.xorg.libXext}/lib:${nixpkgs.xorg.libX11}/lib:${nixpkgs.xorg.libXrandr}/lib";

  buildInputs = [ nixpkgs.gmp ];
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
