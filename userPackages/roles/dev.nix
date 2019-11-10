pkgs:
with pkgs; [
  meld
  gnumake cmake automake
  cloc
  gitAndTools.gitFull
  gitAndTools.tig
  pass-git-helper

  vscode-with-extensions

  python python3

  stack cabal-install cabal2nix
] ++ (with pkgs.haskellPackages; [
  # cabal-install
  ghc hlint pandoc
  hdevtools
])
