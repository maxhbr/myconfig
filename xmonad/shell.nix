{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "myxmonadEnv";
  buildInputs = [ gmp
                  xorg.libXinerama
                  xorg.libXext
                  xorg.libX11
                  xorg.libXrandr
                  xorg.libXft ];
}
