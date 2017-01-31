let
  simple-config = {
    allowUnfree = true;
  };

  pkgs = (import (fetchTarball http://nixos.org/channels/nixos-16.09/nixexprs.tar.xz) { config = simple-config; });
  unstable = (import (fetchTarball http://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) { config = simple-config; });
  # unstabler = (import (fetchTarball http://nixos.org/channels/nixpkgs-unstable/nixexprs.tar.xz) { config = simple-config; });

in simple-config // {
  packageOverrides = super: let self = super.pkgs; in {
    inherit (unstable) ranger;
    inherit (unstable) tmux;
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
