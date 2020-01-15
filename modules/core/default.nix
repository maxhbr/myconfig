{...}: {
  imports = [
    ./nixos.core
    ./vim
    ./zsh
    ./tmux
    ./pass
    ./git
    ./nixos.networking
    ./nixos.nix.nix
    ./nixos.nixpkgs-unstable
    ./other-dotfiles
    ./user.mhuber.nix
  ];
}
