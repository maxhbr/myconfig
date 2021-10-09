{
  description = "my vim flake";

  inputs = { nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable"; };

  outputs = { self, nixpkgs }:
    let  
      pkgs = import nixpkgs { system = "x86_64-linux"; };
      myvim =
        (pkgs.vim_configurable.override { python = pkgs.python3; }).customize {
          name = "vim";
          wrapGui = true;
          vimrcConfig = {
            # customRC = builtins.readFile ./vimrc;
            customRC = ''
            '';
            vam.knownPlugins = pkgs.vimPlugins;
            vam.pluginDictionaries = [{
              names = [
                # "Vundle-vim"
                # "YUNOcommit-vim"
                # # "YouCompleteMe"
                # "airline"
                # "colors-solarized"
                # "ctrlp"
                # "easy-align"
                # "easymotion"
                # "fugitive"
                # "ghcid"
                # # "ghcmod"
                # "gitgutter"
                # "haskell-vim"
                # "haskellconceal"
                # "hoogle"
                # "idris-vim"
                # "neco-ghc"
                # "nerdcommenter"
                # "nerdtree"
                # "quickfixstatus"
                # "quickrun"
                # "rainbow_parentheses"
                # "rust-vim"
                # "shabadou-vim"
                # "signature"
                # "surround"
                # "syntastic"
                # "table-mode"
                # "tabmerge"
                # "tagbar"
                # "taglist"
                # "thumbnail-vim"
                # "undotree"
                # "vim-addon-nix"
                # "vim-autoformat"
                # "vim-gista"
                # "vim-hardtime"
                # "vim-hier"
                # "vim-hindent"
                # "vim-orgmode"
                # "vim-racer"
                # "vim-xkbswitch"
                # "vimproc-vim"
                # "watchdogs"
                # "webapi-vim"
              ];
            }];

          };
        };

      myvimEnv = pkgs.lib.lowPrio (with pkgs;
        buildEnv {
          name = "myvim-env";
          ignoreCollisions = true;
          paths = [
            myvim

            ctags

            aspell
            aspellDicts.de
            aspellDicts.en

            shellcheck
          ];
        });

    in {
      packages.x86_64-linux = {
          myvim = myvim;
          myvimEnv = myvimEnv;
        };
      defaultPackage.x86_64-linux = self.packages.x86_64-linux.myvim;
      defaultApp = {
        type = "app";
        program = "${self.defaultPackage.x86_64-linux}/bin/gvim";
      };
      nixosModule = { config, lib, pkgs, ... }: {
        config = {
          environment = { variables = { EDITOR = "vim"; }; };
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
              in { home.packages = with pkgs; [ myvimEnv ]; })
          ];
        };
      };
    };
}
