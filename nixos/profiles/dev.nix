{ config, pkgs, ... }:

let
  hsPackages = with pkgs.haskellPackages; [
    cabal-install
    ghc hlint pandoc pointfree pointful hdevtools
  ];
in {
  environment.systemPackages = with pkgs; [
    meld
    leiningen clojure
    stack cabal-install cabal2nix
    python python3
    ruby
    gnumake cmake automake

    gitAndTools.gitFull
    gitAndTools.tig
  ] ++ hsPackages;
}
