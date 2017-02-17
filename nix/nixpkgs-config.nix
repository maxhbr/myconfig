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
    inherit (unstable) haskellPackages;
    inherit (unstable) dmenu;
    inherit (unstable) mutt-with-sidebar;
  };

  pkgsWithUnstables = pkgs // unstables;

  callEnv = path: import path {
    pkgs = pkgsWithUnstables;
  };

  cliEnv = callEnv ./envs/cliEnv.nix;
  adminEnv = callEnv ./envs/adminEnv.nix;
  coreEnv = pkgsWithUnstables.buildEnv {
    name = "coreEnv";
    paths = with pkgsWithUnstables; [
      cliEnv
      adminEnv
      wget curl
      git git-lfs
    ];
  };
  muttEnv = callEnv ./envs/muttEnv.nix;
  xmonadEnv = callEnv ./envs/xmonadEnv.nix;
  imageworkEnv = callEnv ./envs/imageworkEnv.nix;
  devEnv = callEnv ./envs/devEnv.nix;
  workEnv = callEnv ./envs/workEnv.nix;

in simple-config // {
  packageOverrides = super: let self = super.pkgs; in unstables // {
    # envs (i.e. groups of packages)
    inherit cliEnv adminEnv coreEnv muttEnv xmonadEnv imageworkEnv devEnv workEnv;

    # texEnv = (pkgs.texLiveAggregationFun {
    #   paths = [
    #     pkgs.texLive pkgs.texLiveExtra
    #     pkgs.texLiveBeamer
    #     pkgs.texLiveCMSuper
    #   ];
    # });
  };
}
