{pkgsWithUnstables, unstable}:
let
  mkEnv = name: paths: pkgsWithUnstables.buildEnv {
    inherit name paths;
    ignoreCollisions = true;
  };
in with pkgsWithUnstables; rec {

  adminEnv = mkEnv "adminEnv" [
    htop
    iftop iptraf-ng iotop
    mkpasswd pwgen
    usbutils
  ];

  cliEnv = mkEnv "cliEnv" [
    ranger
    emacs vim
    elinks w3m
    tmux
    manpages
    taskwarrior
    pass
  ];

  coreEnv = mkEnv "coreEnv" [
    cliEnv
    adminEnv
    wget curl
    git git-lfs
  ];

  muttEnv = mkEnv "muttEnv" [
    neomutt # mutt-with-sidebar
    offlineimap msmtp gnupg abook notmuch urlview
    sxiv
    procmail
  ];

  desktopEnv = mkEnv "desktopEnv" [
    arandr
    xlibs.xmodmap xlibs.xset xlibs.setxkbmap
    xclip
    imagemagick
  # gui applications
    # luakit
    mupdf zathura llpp
    feh scrot
    gnome3.file-roller
    mplayer
    xdotool
  ];

  xmonadEnv = mkEnv "xmonadEnv" ([
    unstable.dmenu unclutter
    xss-lock
  ] ++ (with unstable.haskellPackages; [
    xmonad xmobar yeganesh
  ]));

  imageworkEnv = mkEnv "imageworkEnv" [
    gimp-with-plugins
    rawtherapee
    geeqie
    #krita

    inkscape

    blender
    librecad
  ];

  devEnv = let
    myclojureenv = pkgs.myEnvFun {
      name = "myclojureenv";
      buildInputs = [
        leiningen clojure
      ];
    };
    mypythonenv = pkgs.myEnvFun {
      name = "mypythonenv";
      buildInputs = [
        python python3 python35Packages.bpython
      ];
    };
    myrubyenv = pkgs.myEnvFun {
      name = "myrubyenv";
      buildInputs = [
        ruby
      ];
    };
  in mkEnv "devEnv" ([
    meld
    unstable.stack unstable.cabal-install unstable.cabal2nix
    gnumake cmake automake

    myclojureenv mypythonenv myrubyenv

    cloc

    gitAndTools.gitFull
    gitAndTools.tig
  ] ++ (with unstable.haskellPackages; [
    # cabal-install
    ghc hlint pandoc
    pointfree pointful
    hdevtools
  ]));

  workEnv = mkEnv "workEnv" [
    openvpn networkmanager_openvpn
    rdesktop citrix_receiver
    openjdk maven thrift gradle
    libreoffice
  ];
}
