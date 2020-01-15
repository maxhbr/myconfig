{...}: {
  imports = [
    ../modules/nixos.core
    ../modules/vim
    ../modules/zsh
    ../modules/tmux
    ../modules/pass
    ../modules/git
    ../modules/nixos.networking
    ../modules/nixos.nix.nix
    ../modules/nixos.nixpkgs-unstable
    ../modules/other-dotfiles
    ../modules/user.mhuber.nix
  ];
}
