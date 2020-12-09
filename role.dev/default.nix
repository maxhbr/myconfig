{ pkgs, ... }: {
  imports = [
    # modules
    ./dev.core
    ./dev.haskell
    ./dev.iot.nix
    ./dev.network.nix
    # ./dev.tex.nix
    ./programs.license-compliance-toolbox.nix
  ];
}
