# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
let
  stable_json = builtins.fromJSON (builtins.readFile ./nixos-18.03.json);
  unstable_json = builtins.fromJSON (builtins.readFile ./nixos-unstable.json);

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${stable_json.rev}.tar.gz";
    sha256 = stable_json.outputSha256;
  };
  unstableNixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${unstable_json.rev}.tar.gz";
    sha256 = unstable_json.outputSha256;
  };

  # nixpkgs = builtins.fetchTarball http://nixos.org/channels/nixos-18.03/nixexprs.tar.xz;
  # unstableNixpkgs = builtins.fetchTarball http://nixos.org/channels/nixos-unstable/nixexprs.tar.xz;
in
  { overlays ? [] , ... } @ args:
  let
    pkgs = import nixpkgs args;

    callPackage = pkgs.lib.callPackageWith pkgs;

    scripts = callPackage ../scripts {
      inherit background pkgs;
    };
    my-xmonad = pkgs.haskellPackages.callPackage ../xmonad {
      inherit pkgs scripts;
      my-xmonad-misc = callPackage ../xmonad/misc.nix { inherit pkgs; };
      find-cursor = callPackage ../xmonad/find-cursor.nix { inherit pkgs; };
    };
    background = callPackage ../background { inherit pkgs; };
    slim-theme = callPackage ../background/slim-theme {
      inherit background pkgs;
    };

    config = pkgs: {
      allowUnfree = true;
      mplayer.useUnfreeCodecs = true;
      # virtualbox.enableExtensionPack = true;
    };

    allOverlays = let
        baseOverlays = [
          (self: super: {
            unstable = import (unstableNixpkgs + "/pkgs/top-level") (args //
              {
                inherit config;
                localSystem = { system = builtins.currentSystem;};
              }
            );
          })
          (self: super: {
            myconfig = {
              inherit scripts my-xmonad background slim-theme; # nixosSrc nixSrc dotfiles
              all = self.buildEnv {
                name = "myconfig-all";
                paths = [scripts my-xmonad background slim-theme]; # nixosSrc nixSrc dotfiles
                pathsToLink = [ "/share" "/bin" ];
              };
            };
          })
        ];
        overlaysFromFolders = let
            path = ./overlays;
            content = builtins.readDir path;
          in map (n: import (path + ("/" + n)))
               (builtins.filter (n: builtins.match ".*\\.nix" n != null || builtins.pathExists (path + ("/" + n + "/default.nix")))
                 (builtins.attrNames content));
      in
        overlays ++ baseOverlays ++ overlaysFromFolders;
  in
    import (nixpkgs + "/pkgs/top-level") ({
      inherit config;
      localSystem = { system = builtins.currentSystem; };
      overlays = allOverlays;
    }) // {
      # expose some internals to be used in nixos configuration
      # TODO: that should not be necessary
      myconfig-misc = {
        inherit
          nixpkgs unstableNixpkgs
          config;
        overlays = allOverlays;
      };
    }
