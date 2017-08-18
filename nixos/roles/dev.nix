{ config, lib, pkgs, ... }:
let

  unstable = (import <unstable> {});

  myclojureenv = pkgs.myEnvFun {
    name = "clojur";
    buildInputs = with pkgs; [
      leiningen clojure
    ];
  };
  mypythonenv = pkgs.myEnvFun {
    name = "python";
    buildInputs = with pkgs; [
      python python3 python35Packages.bpython
    ];
  };
  myrubyenv = pkgs.myEnvFun {
    name = "ruby";
    buildInputs = with pkgs; [
      ruby
    ];
  };
  myschemeenv = pkgs.myEnvFun {
    name = "scheme";
    buildInputs = with pkgs; [
      chicken
    ];
 };
 myrustenv = pkgs.myEnvFun {
   name = "rust";
     buildInputs = with pkgs; [
       rustc
   ];
 };

in {

  options = {
    myconfig.roles.dev = {
      enable = lib.mkEnableOption "Dev role";
    };
  };

  config = lib.mkIf config.myconfig.roles.dev.enable {
    environment.systemPackages = with pkgs; [
      meld
      unstable.stack unstable.cabal-install unstable.cabal2nix
      gnumake cmake automake

      myclojureenv mypythonenv myrubyenv myschemeenv myrustenv

      cloc

      gitAndTools.gitFull
      gitAndTools.tig
    ] ++ (with unstable.haskellPackages; [
      # cabal-install
      ghc hlint pandoc
      pointfree pointful
      hdevtools
    ]);
  };
}
