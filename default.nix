# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ system ? builtins.currentSystem, pkgs ? import <nixpkgs> { inherit system; }, ... }@args:
# see:
# - https://www.reddit.com/r/NixOS/comments/4btjnf/fully_setting_up_a_custom_private_nix_repository/?st=jfqxd3k1&sh=92cbc8b5
# - http://sandervanderburg.blogspot.de/2014/07/managing-private-nix-packages-outside.html
let
  callPackage = pkgs.lib.callPackageWith (pkgs // self);
  callHaskellPackage = pkgs.haskellPackages.callPackage;

  packageSources = ({dir, name, pattern ? "*", buildPhase ? ""}:
    pkgs.callPackage (
      { ... }:
      pkgs.stdenv.mkDerivation {
        version = "1.0";
        inherit name;

        src = builtins.filterSource
          (path: type: let
             basename = baseNameOf path;
           in if type == "directory" then basename != ".git"
              else if type == "symlink" then builtins.match "^result(|-.*)$" basename == null
              else builtins.match "^((|\..*)\.sw[a-z]|.*~)$" basename == null)
          dir;
        inherit buildPhase;
        installPhase = ''
          mkdir -p $out/${name}
          cp -r ${pattern} $out/${name}
        '';

        meta = with pkgs.stdenv.lib; {
          description = "package for " + name;
          homepage = "https://github.com/maxhbr/myconfig";
          license = licenses.mit;
          platforms = platforms.unix;
          maintainers = [ ];
        };
      }) {}
    );

  self = rec {
    nixSrc = packageSources {
      dir = ./nix;
      name = "nix";
      buildPhase = ''
        sed -i -e '/dotfiles =/ s%= .*%= "${dotfiles}";%' nixpkgs-config.nix
        sed -i -e '/scripts =/ s%= .*%= "${scripts}";%' nixpkgs-config.nix
        sed -i -e '/background =/ s%= .*%= "${background}";%' nixpkgs-config.nix
        sed -i -e '/slim-theme =/ s%= .*%= "${slim-theme}";%' nixpkgs-config.nix
        sed -i -e '/my-xmonad =/ s%= .*%= "${my-xmonad}";%' nixpkgs-config.nix
      '';
    };
    nixosSrc = packageSources {
      dir = ./nixos;
      name = "nixos";
      buildPhase = ''
        sed -i -e '/nixpkgs\.config =/ s%= .*%= import ${nixSrc}/nix/nixpkgs-config.nix;%' core/default.nix
      '';
    };
    dotfiles = packageSources { dir = ./dotfiles; name = "dotfiles"; };
    scripts = callPackage ./scripts { inherit pkgs background; };
    my-xmonad = callHaskellPackage ./xmonad { inherit pkgs; myConfigScripts = scripts; };
    background = callPackage ./background { inherit pkgs; };
    slim-theme = callPackage ./background/slim-theme { inherit pkgs background; };
    myconfig = pkgs.buildEnv {
      name = "myconfig";
      paths = [
        nixosSrc
        nixSrc
        dotfiles
        scripts
        my-xmonad
        background
        slim-theme
      ];
    };
  };
in self
