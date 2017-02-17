{pkgs,unstable}:
with pkgs; let
  name = "devEnv";
  paths = [
    meld
    leiningen clojure
    unstable.stack unstable.cabal-install unstable.cabal2nix
    python python3
    ruby
    gnumake cmake automake

    cloc

    gitAndTools.gitFull
    gitAndTools.tig
  ] ++ (with unstable.haskellPackages; [
    # cabal-install
    ghc hlint pandoc
    #pointfree pointful
    hdevtools
  ]);
in buildEnv { inherit name paths; }
