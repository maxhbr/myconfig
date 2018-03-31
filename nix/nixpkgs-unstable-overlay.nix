self: super:
let
  spec = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  # unstableSrc = super.fetchFromGitHub {
  #   owner = "NixOS";
  #   repo  = "nixpkgs";
  #   inherit (spec.unstable) rev sha256;
  # };
  # unstable = import unstableSrc { inherit (super) config; };
  unstable = import (fetchTarball http://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) { inherit (super) config; };
in {
  inherit unstable;

  # inherit (unstable) ranger tmux;
  # inherit (unstable) vim vimNox vimHugeX;
  # inherit (unstable) rxvt_unicode_with-plugins rxvt_unicode;
  # # inherit (unstable) emacs;
  # # inherit (unstable) git git-lfs gitFull;
  # # gitAndTools.tig = unstable.gitAndTools.tig;
  # # inherit (unstable) gimp-with-plugins;
  # # inherit (unstable) rawtherapee geeqie;
  # inherit (unstable) oh-my-zsh;
  # # inherit (unstable) haskellPackages;
  # inherit (unstable) dmenu;
  # inherit (unstable) mutt-with-sidebar alot;
  # inherit (self.unstable) weechat;
}
