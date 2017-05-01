let
  simple-config = {
    allowUnfree = true;
  };

  pkgs = (import (fetchTarball http://nixos.org/channels/nixos-17.03/nixexprs.tar.xz) { config = simple-config; });
  unstable = (import (fetchTarball http://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) { config = simple-config; });
  # unstabler = (import (fetchTarball http://nixos.org/channels/nixpkgs-unstable/nixexprs.tar.xz) { config = simple-config; });
  
  inherit (unstable) callPackage;

  myOverrides = rec {
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

    # citrix_receiver = callPackage pkgs/citrix-receiver {};

    premake5 = callPackage pkgs/premake5 {};
    otfcc = callPackage pkgs/otfcc { inherit premake5; };
    iosevka = callPackage pkgs/iosevka { inherit otfcc; };
    imposevka = callPackage pkgs/iosevka/imposevka.nix { inherit otfcc; };

    freetype_subpixel = pkgs.freetype.override {
      useEncumberedCode = true;
      useInfinality = false;
    };
  };

  myEnvs = import ./envs.nix {
    pkgsWithUnstables = pkgs // myOverrides;
    inherit unstable;
  };
in simple-config // {
  packageOverrides = super: let self = super.pkgs; in myEnvs // myOverrides;
}
