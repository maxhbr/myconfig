# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs ? import <nixpkgs> { }, stdenv ? pkgs.stdenv, mkDerivation, base
, containers, process, X11, xmonad_0_17_0, xmonad-contrib_0_17_0, callPackage
, my-xmobar, my-mute-telco }:
let
  version = "1.0";
  my-xmonad-scripts = ./bin;
  my-xmonad-share = ./share;
  find-cursor = callPackage ./find-cursor.nix { inherit pkgs; };
in mkDerivation {
  inherit version;
  pname = "my-xmonad";
  src = builtins.filterSource (path: type:
    let basename = baseNameOf path;
    in if type == "directory" then
      (basename != ".stack-work" && basename != "dist" && basename != "bin"
        && basename != "share")
    else if type == "symlink" then
      builtins.match "^result(|-.*)$" basename == null
    else
      (builtins.match "^((|..*).(sw[a-z]|hi|o)|.*~)$" basename == null
        && builtins.match ".sh$" basename == null)) ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ pkgs.makeWrapper ];
  libraryHaskellDepends =
    [ base containers process X11 xmonad_0_17_0 xmonad-contrib_0_17_0 ];
  executableHaskellDepends =
    [ base containers X11 xmonad_0_17_0 xmonad-contrib_0_17_0 ];

  patchPhase = ''
    set -e
    variablesFile=lib/XMonad/MyConfig/Variables.hs

    addAbsoluteBinaryPath() {
      old=$1
      new=$2/bin/$1
      sed -i -e 's%"'$old'%"'$new'%g' $variablesFile
    }

    replaceConfigValue() {
      key=$1
      value=$2
      sed -i -e '/'"$key"' *=/ s%= .*%= "'"$value"'";%' $variablesFile
    }

    replaceConfigValue xmobarCMD "${my-xmobar}/bin/xmobarXmonad"
    replaceConfigValue pathToXmonadBins "${my-xmonad-scripts}/"
    replaceConfigValue pathToXmonadShare "${my-xmonad-share}/"
  '';

  postInstall = ''
    wrapProgram "$out/bin/xmonad" \
      --prefix PATH ":" "${pkgs.dmenu}/bin" \
      --prefix PATH ":" "${pkgs.haskellPackages.yeganesh}/bin" \
      --prefix PATH ":" "${pkgs.pass}/bin" \
      --prefix PATH ":" "${find-cursor}/bin" \
      --prefix PATH ":" "${pkgs.xdotool}/bin" \
      --prefix PATH ":" "${pkgs.xorg.xf86inputsynaptics}/bin" \
      --prefix PATH ":" "${pkgs.xorg.xkill}/bin" \
      --prefix PATH ":" "${pkgs.xrandr-invert-colors}/bin" \
      --prefix PATH ":" "${pkgs.autorandr}/bin" \
      --prefix PATH ":" "${pkgs.feh}/bin" \
      --prefix PATH ":" "${pkgs.unclutter}/bin" \
      --prefix PATH ":" "${pkgs.htop}/bin" \
      --prefix PATH ":" "${pkgs.pavucontrol}/bin" \
      --prefix PATH ":" "${pkgs.light}/bin" \
      --prefix PATH ":" "${my-mute-telco}/bin"
  '';

  description = "my xmonad configuration";
  license = pkgs.lib.licenses.mit;
}
