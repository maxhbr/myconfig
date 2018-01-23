{ config, lib, pkgs, ... }:

let
 #  myclojureenv = pkgs.myEnvFun {
 #    name = "clojure";
 #    buildInputs = with pkgs; [
 #      leiningen clojure
 #    ];
 #  };
 #  mypythonenv = pkgs.myEnvFun {
 #    name = "python";
 #    buildInputs = with pkgs; [
 #      python python3 python35Packages.bpython
 #    ];
 #  };
 #  myrubyenv = pkgs.myEnvFun {
 #    name = "ruby";
 #    buildInputs = with pkgs; [
 #      ruby
 #    ];
 #  };
 #  myschemeenv = pkgs.myEnvFun {
 #    name = "scheme";
 #    buildInputs = with pkgs; [
 #      chicken
 #    ];
 # };
 # myrustenv = pkgs.myEnvFun {
 #   name = "rust";
 #     buildInputs = with pkgs; [
 #       rustc
 #   ];
 # };

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
        nixpkgs.overlays =
          [(self: super:
            let
              version = "0.9.3";
              name = "thrift-${version}";
            in {
              thrift93 = super.thrift.overrideAttrs ( oldAttrs: {
                version = version;
                name = name;

                src = self.fetchurl {
                  url = "http://archive.apache.org/dist/thrift/${version}/${name}.tar.gz";
                  sha256 = "17lnchan9q3qdg222rgjjai6819j9k755s239phdv6n0183hlx5h";
                };
              });
            })
           (self: super: {
              idea-ultimate = super.idea.idea-ultimate.overrideDerivation (innersuper: rec {
                name = "idea-ultimate-${version}";
                version = "2017.3.3";
                src = super.fetchurl {
                  url = "https://download.jetbrains.com/idea/ideaIU-${version}.tar.gz";
                  sha256 = "0mbyb31kc9d52hnbn9dclbw0q9y0c6pi8351rbq68jphslm3i9q5";
                };
              });
            })
          ];
        environment.systemPackages = with pkgs; [
          openvpn networkmanager_openvpn
          rdesktop
          # citrix_receiver
          unstable.openjdk unstable.maven unstable.gradle
          # libreoffice
          hipchat franz
          p7zip
          thrift93
          idea-ultimate
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

          # myclojureenv mypythonenv myrubyenv myschemeenv myrustenv

          cloc

          gitAndTools.gitFull
          gitAndTools.tig
        ] ++ (with pkgs.unstable.haskellPackages; [
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
