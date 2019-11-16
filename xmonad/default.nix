# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv, scripts, mkDerivation, base, containers, process, X11, xmonad, xmonad-contrib, callPackage }:
let
  version = "1.0";
  my-xmonad-misc = callPackage ./misc.nix {
    inherit pkgs;
  };
  find-cursor = callPackage ./find-cursor.nix {
    inherit pkgs;
  };
in mkDerivation {
  inherit version;
  pname = "my-xmonad";
  src = builtins.filterSource
    (path: type: let
      basename = baseNameOf path;
      in if type == "directory" then (basename != ".stack-work" &&
                                      basename != "dist" &&
                                      basename != "bin" &&
                                      basename != "share")
        else if type == "symlink" then builtins.match "^result(|-.*)$" basename == null
          else (builtins.match "^((|\..*)\.(sw[a-z]|hi|o)|.*~)$" basename == null &&
                builtins.match "\.sh$" basename == null))
    ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends    = [base containers process X11 xmonad xmonad-contrib];
  executableHaskellDepends = [base containers         X11 xmonad xmonad-contrib];

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

    addAbsoluteBinaryPath xmobar ${pkgs.haskellPackages.xmobar}
    addAbsoluteBinaryPath urxvtc ${pkgs.rxvt_unicode_with-plugins}
    addAbsoluteBinaryPath urxvtd ${pkgs.rxvt_unicode_with-plugins}
    addAbsoluteBinaryPath bash ${pkgs.bash}
    addAbsoluteBinaryPath zsh ${pkgs.zsh}
    addAbsoluteBinaryPath emacs ${pkgs.emacs}
    addAbsoluteBinaryPath dmenu_path ${pkgs.dmenu} #$#{pkgs.unstable.dmenu}
    addAbsoluteBinaryPath yeganesh ${pkgs.haskellPackages.yeganesh} #$#{pkgs.unstable.haskellPackages.yeganesh}
    addAbsoluteBinaryPath passmenu ${pkgs.pass}
    addAbsoluteBinaryPath firefox ${pkgs.firefox}
    addAbsoluteBinaryPath chromium-browser ${pkgs.chromium}
    addAbsoluteBinaryPath find-cursor ${find-cursor}
    addAbsoluteBinaryPath xdotool ${pkgs.xdotool}
    addAbsoluteBinaryPath synclient ${pkgs.xorg.xf86inputsynaptics}
    addAbsoluteBinaryPath xrandr-invert-colors ${pkgs.xrandr-invert-colors}
    addAbsoluteBinaryPath feh ${pkgs.feh}
    addAbsoluteBinaryPath unclutter ${pkgs.unclutter}
    addAbsoluteBinaryPath htop ${pkgs.htop}
    addAbsoluteBinaryPath pavucontrol ${pkgs.pavucontrol}
    addAbsoluteBinaryPath light ${pkgs.light}

    replaceConfigValue pathToXmobarConfig "${my-xmonad-misc}/share/xmobarrc"
    replaceConfigValue pathToXmobarMinConfig "${my-xmonad-misc}/share/xmobarrc.minimal"
    replaceConfigValue pathToXmonadBins "${my-xmonad-misc}/bin/"
    replaceConfigValue pathToXmonadShare "${my-xmonad-misc}/share/"
    replaceConfigValue pathToMyconfigBins "${scripts}/bin/"
  '';

  description = "my xmonad configuration";
  license = stdenv.lib.licenses.mit;
}
