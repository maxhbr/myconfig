let
  simple-config = {
    allowUnfree = true;
  };

  pkgs = (import (fetchTarball http://nixos.org/channels/nixos-16.09/nixexprs.tar.xz) { config = simple-config; });
  unstable = (import (fetchTarball http://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) { config = simple-config; });
  # unstabler = (import (fetchTarball http://nixos.org/channels/nixpkgs-unstable/nixexprs.tar.xz) { config = simple-config; });

  unstables = {
    inherit (unstable) ranger tmux;
    inherit (unstable) vim vimNox vimHugeX;
    inherit (unstable) rxvt_unicode_with-plugins rxvt_unicode;
    # inherit (unstable) emacs;
    # inherit (unstable) git git-lfs gitFull;
    # gitAndTools.tig = unstable.gitAndTools.tig;
    inherit (unstable) gimp-with-plugins;
    # inherit (unstable) rawtherapee geeqie;
    inherit (unstable) oh-my-zsh;
    # inherit (unstable) haskellPackages;
    inherit (unstable) dmenu;
    inherit (unstable) mutt-with-sidebar;
  };

  myenvs = import ./envs.nix {
    pkgsWithUnstables = pkgs // unstables;
    inherit unstable;
  };
in simple-config // {
  packageOverrides = super: let self = super.pkgs; in myenvs // unstables;
}
