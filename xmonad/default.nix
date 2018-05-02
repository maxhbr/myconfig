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
                                      basename != "share" )
        else if type == "symlink" then builtins.match "^result(|-.*)$" basename == null
          else (builtins.match "^((|\..*)\.(sw[a-z]|hi|o)|.*~)$" basename == null &&
                builtins.match "\.sh$" basename == null))
    ./.;
  isLibrary = true;
  isExecutable = false;
  libraryHaskellDepends = [
    base containers process X11 xmonad xmonad-contrib
  ];
  executableHaskellDepends = [
    base containers X11 xmonad xmonad-contrib
  ];

  patchPhase = ''
    set -e
    variablesFile=lib/XMonad/MyConfig/Variables.hs

    replace() {
      old=$1
      new=$2/bin/$1
      sed -i -e 's%"'$old'%"'$new'%g' $variablesFile
    }

    replace xmobar ${pkgs.haskellPackages.xmobar}
    replace urxvtc ${pkgs.rxvt_unicode_with-plugins}
    replace urxvtd ${pkgs.rxvt_unicode_with-plugins}
    replace bash ${pkgs.bash}
    replace zsh ${pkgs.zsh}
    replace dmenu_path ${pkgs.unstable.dmenu}
    replace yeganesh ${pkgs.unstable.haskellPackages.yeganesh}
    replace passmenu ${pkgs.pass}
    replace firefox ${pkgs.firefox}
    replace chromium-browser ${pkgs.chromium}
    replace find-cursor ${find-cursor}
    replace xdotool ${pkgs.xdotool}
    replace synclient ${pkgs.xorg.xf86inputsynaptics}
    replace xrandr-invert-colors ${pkgs.xrandr-invert-colors}
    replace feh ${pkgs.feh}
    replace unclutter ${pkgs.unclutter}
    replace htop ${pkgs.htop}
    replace pavucontrol ${pkgs.pavucontrol}

    sed -i -e '/pathToXmobarConfig *=/ s%= .*%= "${my-xmonad-misc}/share/xmobarrc";%' $variablesFile
    sed -i -e '/pathToXmonadBins *=/ s%= .*%= "${my-xmonad-misc}/bin/";%' $variablesFile
    sed -i -e '/pathToXmonadShare *=/ s%= .*%= "${my-xmonad-misc}/share/";%' $variablesFile
    sed -i -e '/pathToMyconfigBins *=/ s%= .*%= "${scripts}/bin/";%' $variablesFile
  '';

  description = "my xmonad configuration";
  license = stdenv.lib.licenses.mit;
}
