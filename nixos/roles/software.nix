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
    myconfig.roles.work = {
      enable = lib.mkEnableOption "Work role";
    };
    myconfig.roles.dev = {
      enable = lib.mkEnableOption "Dev role";
    };
    myconfig.roles.imagework = {
      enable = lib.mkEnableOption "Imagework role";
    };
    myconfig.roles.wine = {
      enable = lib.mkEnableOption "Wine role";
    };
    myconfig.roles.tex = {
      enable = lib.mkEnableOption "Tex role";
    };
    myconfig.roles.sundtek = {
      enable = lib.mkEnableOption "Sundtek role";
    };
  };

  imports = [
################################################################################
    { # work
      config = lib.mkIf config.myconfig.roles.work.enable {
        environment.systemPackages = with pkgs; [
          openvpn networkmanager_openvpn
          rdesktop
          # citrix_receiver
          openjdk unstable.maven thrift gradle
          libreoffice
        ];
      };
    }
################################################################################
    { # dev
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
################################################################################
    { # imagework
      config = lib.mkIf config.myconfig.roles.imagework.enable {
        environment.systemPackages = with pkgs; [
          gimp-with-plugins
          rawtherapee
          geeqie
          #krita

          inkscape

          blender
          librecad
        ];
      };
    }
################################################################################
    { # wine 
      config = lib.mkIf config.myconfig.roles.wine.enable {
        environment.systemPackages = with pkgs; [
          wineStaging
          winetricks
        ];
      };
    }
################################################################################
    { # tex 
      config = lib.mkIf config.myconfig.roles.tex.enable {
        environment.systemPackages = with pkgs; [
          (pkgs.texLiveAggregationFun {
            paths = [
              pkgs.texLive pkgs.texLiveExtra
              pkgs.texLiveBeamer
              pkgs.texLiveCMSuper
            ];
          })
        ];
      };
    }
################################################################################
    { # sundtek
      config = lib.mkIf config.myconfig.roles.sundtek.enable {
        environment.systemPackages = with pkgs; [
          sundtek
        ];
      };
    }
  ];
}
