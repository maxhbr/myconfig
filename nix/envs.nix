{pkgsWithUnstables, unstable}:
let
  mkEnv = name: paths: pkgsWithUnstables.buildEnv {
    inherit name paths;
    ignoreCollisions = true;
  };
in with pkgsWithUnstables; rec {

  adminEnv = mkEnv "adminEnv" [
    htop
    iftop iptraf-ng iotop mtr bind
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
    unzip
    nox
    tree
  ];

  muttEnv = mkEnv "muttEnv" [
    neomutt # mutt-with-sidebar
    offlineimap msmtp gnupg abook notmuch urlview
    sxiv
    procmail
  ];

  desktopEnv = mkEnv "desktopEnv" [
    arandr xrandr-invert-colors
    xlibs.xmodmap xlibs.xset xlibs.setxkbmap
    xclip
    imagemagick
  # gui applications
    mupdf zathura llpp
    feh scrot
    xarchiver # gnome3.file-roller
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
      name = "clojur";
      buildInputs = [
        leiningen clojure
      ];
    };
    mypythonenv = pkgs.myEnvFun {
      name = "python";
      buildInputs = [
        python python3 python35Packages.bpython
      ];
    };
    myrubyenv = pkgs.myEnvFun {
      name = "ruby";
      buildInputs = [
        ruby
      ];
    };
    myschemeenv = pkgs.myEnvFun {
      name = "scheme";
      buildInputs = [
        chicken
      ];
   };
   myrustenv = pkgs.myEnvFun {
     name = "rust";
       buildInputs = [
         rustc
     ];
   };
  in mkEnv "devEnv" ([
    meld
    unstable.stack unstable.cabal-install unstable.cabal2nix
    gnumake cmake automake

    myclojureenv mypythonenv myrubyenv myschemeenv myrustenv

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
    rdesktop
    # citrix_receiver
    openjdk unstable.maven thrift gradle
    libreoffice
  ];
}
