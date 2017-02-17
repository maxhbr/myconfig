{pkgs}:
with pkgs; let
  name = "devEnv";
  paths = [
    meld
    leiningen clojure
    stack cabal-install cabal2nix
    python python3
    ruby
    gnumake cmake automake

    cloc

    gitAndTools.gitFull
    gitAndTools.tig
  ] ++ (with haskellPackages; [
    # cabal-install
    ghc hlint pandoc
    #pointfree pointful
    hdevtools
  ]);
in buildEnv { inherit name paths; }
