pkgs:
let
  unstable = (import <unstable> {
    config = {allowUnfree = true;};
  });
in {
  allowUnfree = true;
  overrides = {
    # freetype_subpixel = pkgs.freetype.override {
    #   useEncumberedCode = true;
    #   useInfinality = false;
    # };

    inherit (unstable) ranger tmux;
    inherit (unstable) vim vimNox vimHugeX;
    inherit (unstable) rxvt_unicode_with-plugins rxvt_unicode;
    # inherit (unstable) emacs;
    # inherit (unstable) git git-lfs gitFull;
    # gitAndTools.tig = unstable.gitAndTools.tig;
    # inherit (unstable) gimp-with-plugins;
    # inherit (unstable) rawtherapee geeqie;
    inherit (unstable) oh-my-zsh;
    # inherit (unstable) haskellPackages;
    inherit (unstable) dmenu;
    inherit (unstable) mutt-with-sidebar alot;
    inherit (unstable) weechat;

    # inherit (unstable) citrix_receiver;
  };
}
