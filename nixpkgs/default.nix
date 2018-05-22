# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# see:
# - https://www.reddit.com/r/NixOS/comments/4btjnf/fully_setting_up_a_custom_private_nix_repository/?st=jfqxd3k1&sh=92cbc8b5
# - http://sandervanderburg.blogspot.de/2014/07/managing-private-nix-packages-outside.html

let
  stable_json = builtins.fromJSON (builtins.readFile ./stable.json);
  unstable_json = builtins.fromJSON (builtins.readFile ./unstable.json);

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${stable_json.rev}.tar.gz";
    # sha256 = stable_json.outputSha256;
  };
  unstableNixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${unstable_json.rev}.tar.gz";
    # sha256 = unstable_json.outputSha256;
  };
in
  { config ? {}
  , overlays ? []
  , ...
  } @ args:
  let
    pkgs = import nixpkgs args;

    callPackage = pkgs.lib.callPackageWith pkgs;
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

      nixSrc = packageSources {
        dir = ../nix;
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
        dir = ../nixos;
        name = "nixos";
        buildPhase = ''
          sed -i -e '/nixpkgs\.config =/ s%= .*%= import ${nixSrc}/nix/nixpkgs-config.nix;%' core/default.nix
        '';
      };
      dotfiles = packageSources { dir = ../dotfiles; name = "dotfiles"; };
      scripts = callPackage ../scripts {
        inherit background pkgs;
      };
      my-xmonad = callHaskellPackage ../xmonad {
        inherit pkgs scripts;
        my-xmonad-misc = callPackage ../xmonad/misc.nix { inherit pkgs; };
        find-cursor = callPackage ../xmonad/find-cursor.nix { inherit pkgs; };
      };
      background = callPackage ../background { inherit pkgs; };
      slim-theme = callPackage ../background/slim-theme {
        inherit background pkgs;
      };
  in
    import (nixpkgs + "/pkgs/top-level") (args // {
      localSystem = { system = builtins.currentSystem; };
      overlays = let
          baseOverlays = [
            (self: super: {
              unstable = import (unstableNixpkgs + "/pkgs/top-level") (args //
                { localSystem = { system = builtins.currentSystem; }; }
              );
            })
            (self: super: {
              myconfig = {
                # nixosSrc
                # nixSrc
                # dotfiles
                inherit scripts my-xmonad background slim-theme;
              };
              # myconfig = self.buildEnv {
              #   name = "myconfig";
              #   paths = [
              #     nixosSrc
              #     nixSrc
              #     dotfiles
              #     scripts
              #     my-xmonad
              #     background
              #     slim-theme
              #   ];
              # };
            })
          ];
          path = ./overlays;
          content = builtins.readDir path;
          overlaysFromFolders = map (n: import (path + ("/" + n)))
                                  (builtins.filter (n: builtins.match ".*\\.nix" n != null || builtins.pathExists (path + ("/" + n + "/default.nix")))
                                     (builtins.attrNames content));
        in
          overlays ++ baseOverlays ++ overlaysFromFolders;
      config = pkgs: {
        allowUnfree = true;
        mplayer.useUnfreeCodecs = true;
        # packageOverrides = myconfig.overlays;
        # virtualbox.enableExtensionPack = true;
      };
    })
