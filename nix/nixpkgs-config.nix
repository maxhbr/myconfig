{ pkgs, ... }:

let
  simple-config = {
    allowUnfree = true;
  };

  unstable = (import <unstable> { config = simple-config; });
  unstabler = (import <unstabler> { config = simple-config; });

in simple-config // {
  packageOverrides = super: let self = super.pkgs; in {
    inherit (unstable) ranger;
    inherit (unstabler) tmux;
    # inherit (unstable) git;
    # inherit (unstable) git-lfs;
    inherit (unstable) rxvt_unicode_with-plugins;
    inherit (unstable) rxvt_unicode;
    # # inherit (unstable.gitAndTools) gitFull;
    # gitAndTools.tig = unstable.gitAndTools.tig;
    inherit (unstable) gimp-with-plugins;
    # inherit (unstable) rawtherapee;
    # inherit (unstable) geeqie;
    inherit (unstable) oh-my-zsh;
    inherit (unstable) haskellPackages;
    inherit (unstable) dmenu;
  };
}
