{ pkgs, mkDerivation, base, containers, process, stdenv, X11, xmonad, xmonad-contrib, myConfigScripts }:
let
  binDir = ./bin;
  xmobarrc = ./xmobarrc;
in mkDerivation {
  pname = "maxhbr-xmonad-config";
  version = "1.0";
  src = builtins.filterSource
    (path: type: let
      basename = baseNameOf path;
      in if type == "directory" then basename != ".stack-work"
        else if type == "symlink" then builtins.match "^result(|-.*)$" basename == null
          else builtins.match "^((|\..*)\.(sw[a-z]|hi|o)|.*~)$" basename == null)
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
    cmdsFile=lib/XMonad/MyConfig/Commands.hs

    replace() {
      old=$1
      new=$2/bin/$1
      sed -i -e 's%"'$old'%"'$new'%g' $cmdsFile
    }

    replace urxvtc ${pkgs.rxvt_unicode_with-plugins}
    replace urxvtd ${pkgs.rxvt_unicode_with-plugins}
    replace bash ${pkgs.bash}
    replace zsh ${pkgs.zsh}
    replace dmenu_path ${pkgs.dmenu}
    replace yeganesh ${pkgs.haskellPackages.yeganesh}
    replace passmenu ${pkgs.pass}
    replace find-cursor ${pkgs.find-cursor}
    replace xdotool ${pkgs.xdotool}
    replace synclient ${pkgs.xorg.xf86inputsynaptics}
    replace xrandr-invert-colors ${pkgs.xrandr-invert-colors}
    replace feh ${pkgs.feh}
    replace unclutter ${pkgs.unclutter}
    replace htop ${pkgs.htop}

    sed -i -e '/pathToXmonadBins *=/ s%= .*%= "${binDir}/";%' $cmdsFile
    sed -i -e '/pathToMyconfigBins *=/ s%= .*%= "${myConfigScripts}/bin/";%' $cmdsFile
    sed -i -e 's%~/.xmonad/xmobarrc%${xmobarrc}%g' lib/XMonad/MyConfig.hs
  '';

  description = "my xmonad configuration";
  license = stdenv.lib.licenses.mit;
}
