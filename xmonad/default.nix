# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, mkDerivation, base, containers, process, stdenv, X11, xmonad, xmonad-contrib, scripts, my-xmonad-misc, find-cursor }:
let
  version = "1.0";
in mkDerivation {
  inherit version;
  pname = "my-xmonad-${version}";
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
  isExecutable = false;
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

    addAbsoluteBinaryPath xmobar ${pkgs.haskellPackages.xmobar}
    addAbsoluteBinaryPath urxvtc ${pkgs.rxvt_unicode_with-plugins}
    addAbsoluteBinaryPath urxvtd ${pkgs.rxvt_unicode_with-plugins}
    addAbsoluteBinaryPath bash ${pkgs.bash}
    addAbsoluteBinaryPath zsh ${pkgs.zsh}
    addAbsoluteBinaryPath dmenu_path ${pkgs.unstable.dmenu}
    addAbsoluteBinaryPath yeganesh ${pkgs.unstable.haskellPackages.yeganesh}
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

    sed -i -e '/pathToXmobarConfig *=/ s%= .*%= "${my-xmonad-misc}/share/xmobarrc";%' $variablesFile
    sed -i -e '/pathToXmobarMinConfig *=/ s%= .*%= "${my-xmonad-misc}/share/xmobarrc.minimal";%' $variablesFile
    sed -i -e '/pathToXmonadBins *=/ s%= .*%= "${my-xmonad-misc}/bin/";%' $variablesFile
    sed -i -e '/pathToXmonadShare *=/ s%= .*%= "${my-xmonad-misc}/share/";%' $variablesFile
    sed -i -e '/pathToMyconfigBins *=/ s%= .*%= "${scripts}/bin/";%' $variablesFile
  '';

  description = "my xmonad configuration";
  license = stdenv.lib.licenses.mit;
}
