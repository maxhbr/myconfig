{ pkgs, lib, config, ... }:
let
  myvim =
    (pkgs.vim_configurable.override { python = pkgs.python3; }).customize {
      name = "vim";
      wrapGui = true;
      vimrcConfig = {
        customRC = builtins.readFile ./vimrc;
        vam.knownPlugins = pkgs.vimPlugins;
        vam.pluginDictionaries = [{
          names = [
            "Vundle-vim"
            "YUNOcommit-vim"
            # "YouCompleteMe"
            "airline"
            "colors-solarized"
            "ctrlp"
            "easy-align"
            "easymotion"
            "fugitive"
            "ghcid"
            # "ghcmod"
            "gitgutter"
            "haskell-vim"
            "haskellconceal"
            "hoogle"
            "idris-vim"
            "neco-ghc"
            "nerdcommenter"
            "nerdtree"
            "quickfixstatus"
            "quickrun"
            "rainbow_parentheses"
            "rust-vim"
            "shabadou-vim"
            "signature"
            "surround"
            "syntastic"
            "table-mode"
            "tabmerge"
            "tagbar"
            "taglist"
            "thumbnail-vim"
            "undotree"
            "vim-addon-nix"
            "vim-autoformat"
            "vim-gista"
            "vim-hardtime"
            "vim-hier"
            "vim-hindent"
            "vim-orgmode"
            "vim-racer"
            "vim-xkbswitch"
            "vimproc-vim"
            "watchdogs"
            "webapi-vim"
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
  config = {
    environment = { variables = { EDITOR = "vim"; }; };
    home-manager.sharedModules =
      [ ({ config, ... }: { home.packages = with pkgs; [ myvimEnv ]; }) ];
  };
}
