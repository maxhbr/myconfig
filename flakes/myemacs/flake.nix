{
  description = "my doom-emacs configuration";

  inputs = {
    home-manager.url = "github:rycee/home-manager";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-doom-emacs.url = "github:nix-community/nix-doom-emacs";
    nix-doom-emacs.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nix-doom-emacs, nixpkgs, ... }:
    let
      doomPrivateDir = let
        pkgs = import nixpkgs { system = "x86_64-linux"; };
        doomPrivateDeriv = pkgs.stdenv.mkDerivation rec {
          name = "doomPrivateDeriv-1.0";
          src = ./doom.d;
          installPhase = ''
            mkdir -p "$out"
            cp -r * "$out"
          '';
        };
      in "${doomPrivateDeriv}";
      doom-emacs-conf = pkgs: {
        inherit doomPrivateDir;
        extraPackages = [
          pkgs.mu
          pkgs.gnuplot
          (pkgs.nerdfonts.override { fonts = [ "Inconsolata" ]; })
        ];
        extraConfig = ''
          (setq mu4e-mu-binary "${pkgs.mu}/bin/mu")
        '';
        # dependencyOverrides = nix-doom-emacs.inputs;
      };
    in {
      packages.x86_64-linux =
        let pkgs = import nixpkgs { system = "x86_64-linux"; };
        in {
          doom-emacs = pkgs.callPackage nix-doom-emacs (doom-emacs-conf pkgs);
        };
      defaultPackage.x86_64-linux = self.packages.x86_64-linux.doom-emacs;
      defaultApp = {
        type = "app";
        program = "${self.defaultPackage.x86_64-linux}/bin/emacs";
      };
      nixosModule = { config, lib, pkgs, ... }: {
        config = {
          nixpkgs.overlays = [
            (self: super: {
              dune =
                super.dune_1; # see: https://github.com/vlaci/nix-doom-emacs/issues/166
              #emacs =
              #  super.emacs.overrideDerivation (drv: { nativeComp = true; });
            })
          ];
          environment = {
            variables = { EDITOR = "emacs -nw"; };
            # shellAliases = { vim = "emacs -nw"; };
          };
          home-manager.sharedModules = [
            ({ config, ... }:
              let
                xclipedit = pkgs.writeShellScriptBin "xclipedit" ''
                  set -euo pipefail
                  tempfile="$(mktemp)"
                  trap "rm $tempfile" EXIT
                  if [ -t 0 ]; then
                      xclip -out > "$tempfile"
                  else
                      cat > "$tempfile"
                  fi
                  $EDITOR "$tempfile"
                  ${pkgs.xclip}/bin/xclip < "$tempfile"
                '';
              in {
                imports = [
                  # doom emacs
                  nix-doom-emacs.hmModule
                ];
                programs.doom-emacs = (doom-emacs-conf pkgs) // {
                  enable = true;
                };
                home.packages = with pkgs; [
                  # self.defaultPackage.x86_64-linux

                  xclipedit
                  emacs-all-the-icons-fonts

                  aspell
                  aspellDicts.de
                  aspellDicts.en

                  shellcheck
                ];
                programs.zsh.shellAliases = {
                  magit = ''emacs -e "(magit-status \"$(pwd)\")"'';
                };
                home.file = {
                  ".doom.d/imports" = {
                    source = ./doom.d-imports;
                    recursive = true;
                  };
                };
              })
          ];
        };
      };
    };
}
