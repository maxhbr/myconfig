{pkgsWithUnstables, unstable}:
let
  mkEnv = name: paths: pkgsWithUnstables.buildEnv {
    inherit name paths;
    ignoreCollisions = true;
  };
in with pkgsWithUnstables; rec {

  adminEnv = mkEnv "adminEnv" [
    htop iftop iptraf-ng iotop
    mkpasswd
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
    mutt-with-sidebar
    offlineimap msmtp gnupg abook notmuch urlview
  ];

  desktopEnv = lib.lowPrio (mkEnv "desktopEnv" [
    arandr
    xlibs.xmodmap xlibs.xset xlibs.setxkbmap
    xclip
    imagemagick
  # gui applications
    luakit
    mupdf zathura llpp
    feh scrot
    gnome3.file-roller
    mplayer
    xdotool
  ]);

  xmonadEnv = lib.lowPrio (mkEnv "xmonadEnv" ([
    unstable.dmenu unclutter
    slock
  ] ++ (with unstable.haskellPackages; [
    xmonad xmobar yeganesh
  ])));

  imageworkEnv = lib.lowPrio (mkEnv "imageworkEnv" [
    gimp-with-plugins
    rawtherapee
    geeqie

    blender
    librecad

    inkscape
  ]);

  devEnv = lib.lowPrio (mkEnv "devEnv" ([
    meld
    leiningen clojure
    unstable.stack unstable.cabal-install unstable.cabal2nix
    python python3
    ruby
    gnumake cmake automake

    cloc

    gitAndTools.gitFull
    gitAndTools.tig
  ] ++ (with unstable.haskellPackages; [
    # cabal-install
    ghc hlint pandoc
    #pointfree pointful
    hdevtools
  ])));

  workEnv = lib.lowPrio (mkEnv "workEnv" [
    openvpn networkmanager_openvpn
    rdesktop
    openjdk maven thrift gradle
  ]);
}
