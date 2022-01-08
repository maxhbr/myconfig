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
            "ale"
            "airline"
            "colors-solarized"
            "ctrlp"
            "easy-align"
            "easymotion"
            "fugitive"
            "ghcid"
            "gitgutter"
            "idris-vim"
            "neco-ghc"
            "nerdcommenter"
            "nerdtree"
            "quickfixstatus"
            "quickrun"
            "rainbow_parentheses"
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
            "vim-commentary"
            "vim-gista"
            "vim-hardtime"
            "vim-hier"
            "vim-hindent"
            "vim-orgmode"
            "vim-racer"
            "vim-xkbswitch"
            "vimagit"
            "vimproc-vim"
            "vimwiki"
            "watchdogs"
            "webapi-vim"
          ] ++ [
            "stylish-haskell"
            "haskell-vim"
            # "haskellconceal"
            "hoogle"
          ] ++ [ "rust-vim" ];
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
        nixfmt
        shfmt
        stylish-haskell
      ];
    });
in {
  config = {
    environment = {
      variables = { EDITOR = lib.mkForce "vim"; };
      # shellAliases = { emacs = "gvim"; };
    };
    home-manager.sharedModules = [
      ({ config, ... }: {
        home.sessionVariables = { EDITOR = "vim"; };
        home.packages = with pkgs; [ myvimEnv ];
        home.file = { ".gvimrc".source = ./gvimrc; };
      })
      ({ config, ... }:
        let
          nvim-spell-de-utf8-dictionary = builtins.fetchurl {
            url = "http://ftp.vim.org/vim/runtime/spell/de.utf-8.spl";
            sha256 = "1ld3hgv1kpdrl4fjc1wwxgk4v74k8lmbkpi1x7dnr19rldz11ivk";
          };

          nvim-spell-de-utf8-suggestions = builtins.fetchurl {
            url = "http://ftp.vim.org/vim/runtime/spell/de.utf-8.sug";
            sha256 = "0j592ibsias7prm1r3dsz7la04ss5bmsba6l1kv9xn3353wyrl0k";
          };

          nvim-spell-de-latin1-dictionary = builtins.fetchurl {
            url = "http://ftp.vim.org/vim/runtime/spell/de.latin1.spl";
            sha256 = "0hn303snzwmzf6fabfk777cgnpqdvqs4p6py6jjm58hdqgwm9rw9";
          };

          nvim-spell-de-latin1-suggestions = builtins.fetchurl {
            url = "http://ftp.vim.org/vim/runtime/spell/de.latin1.sug";
            sha256 = "0mz07d0a68fhxl9vmy1548vnbayvwv1pc24zhva9klgi84gssgwm";
          };
          nvim-spell-en-utf8-dictionary = builtins.fetchurl {
            url = "http://ftp.vim.org/vim/runtime/spell/en.utf-8.spl";
            sha256 = "0w1h9lw2c52is553r8yh5qzyc9dbbraa57w9q0r9v8xn974vvjpy";
          };

          nvim-spell-en-utf8-suggestions = builtins.fetchurl {
            url = "http://ftp.vim.org/vim/runtime/spell/en.utf-8.sug";
            sha256 = "1v1jr4rsjaxaq8bmvi92c93p4b14x2y1z95zl7bjybaqcmhmwvjv";
          };

          nvim-spell-en-latin1-dictionary = builtins.fetchurl {
            url = "http://ftp.vim.org/vim/runtime/spell/en.latin1.spl";
            sha256 = "0i8q2si2bm8c0556j3c0gjin3bixgs055yqqk1irvz4wszy9w3b2";
          };

          nvim-spell-en-latin1-suggestions = builtins.fetchurl {
            url = "http://ftp.vim.org/vim/runtime/spell/en.latin1.sug";
            sha256 = "00ibcbj2b2krwd5zl9zl671db44k3fl59sz1yymb9ydkpkj9gpp6";
          };
        in {
          home.file = {
            ".vim/spell/de.utf-8.spl".source = nvim-spell-de-utf8-dictionary;
            ".vim/spell/de.utf-8.sug".source = nvim-spell-de-utf8-suggestions;
            ".vim/spell/de.latin1.spl".source = nvim-spell-de-latin1-dictionary;
            ".vim/spell/de.latin1.sug".source =
              nvim-spell-de-latin1-suggestions;
            ".vim/spell/en.utf-8.spl".source = nvim-spell-en-utf8-dictionary;
            ".vim/spell/en.utf-8.sug".source = nvim-spell-en-utf8-suggestions;
            ".vim/spell/en.latin1.spl".source = nvim-spell-en-latin1-dictionary;
            ".vim/spell/en.latin1.sug".source =
              nvim-spell-en-latin1-suggestions;
          };
        })
    ];
  };
}
