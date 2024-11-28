# Partially copied from
#  - https://github.com/srid/nixos-config/blob/master/home/neovim.nix
{ pkgs, lib, config, ... }: {
  imports = [
    ({ ... }:
      lib.mkIf config.programs.neovim.enable {
        programs.neovim = {
          # enable = true;
          defaultEditor = true;
        };
        home-manager.sharedModules = [
          ({ config, ... }: {
            programs.neovim = {
              enable = true;
              extraConfig = ''
                lua << EOF
                ${builtins.readFile ./neovim.lua}
                EOF
              '';
              viAlias = true;
              vimAlias = true;

              extraPackages = [ pkgs.lazygit ];

              # Full list here,
              # https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/editors/vim/plugins/generated.nix
              plugins = with pkgs.vimPlugins; [
                vim-commentary

                nvim-tree-lua
                {
                  plugin = vim-startify;
                  config = "let g:startify_change_to_vcs_root = 0";
                }

                # For working mouse support when running inside tmux
                terminus

                {
                  plugin = lazygit-nvim;
                  type = "lua";
                  config = ''
                    nmap("<leader>gg", ":LazyGit<cr>")
                  '';
                }

                (nvim-treesitter.withPlugins (_: pkgs.tree-sitter.allGrammars))
                # Preferred theme
                tokyonight-nvim
                sonokai
                dracula-vim
                gruvbox
                papercolor-theme

                # File browser
                {
                  plugin = nvim-tree-lua;
                  type = "lua";
                  config = ''
                    require("nvim-tree").setup()
                  '';
                }
                nvim-web-devicons

                {
                  plugin = lualine-nvim;
                  type = "lua";
                  config = ''
                    require('lualine').setup {
                      options = {
                        theme = 'tokyonight'
                      }
                    }
                  '';
                }

                # Buffer tabs
                {
                  plugin = bufferline-nvim;
                  type = "lua";
                  config = ''
                    require("bufferline").setup{ }
                    nmap("<leader>b", ":BufferLineCycleNext<cr>")
                    nmap("<leader>B", ":BufferLineCyclePrev<cr>")
                  '';
                }

                # Developing plugins in Haskell
                nvim-hs-vim

                # Language support
                vim-nix

                # nushell
                null-ls-nvim
                {
                  plugin = nvim-nu;
                  type = "lua";
                  config = ''
                      require'nu'.setup{
                        use_lsp_features = true, -- requires https://github.com/jose-elias-alvarez/null-ls.nvim
                        -- lsp_feature: all_cmd_names is the source for the cmd name completion.
                        -- It can be
                        --  * a string, which is interpreted as a shell command and the returned list is the source for completions (requires plenary.nvim)
                        --  * a list, which is the direct source for completions (e.G. all_cmd_names = {"echo", "to csv", ...})
                        --  * a function, returning a list of strings and the return value is used as the source for completions
                        all_cmd_names = [[nu -c 'help commands | get name | str join "\n"']]
                    }
                  '';
                }

                vim-markdown

                {
                  # :Copilot setup
                  plugin = copilot-vim;
                  config = ''
                    let g:copilot_filetypes = {
                          \ '*': v:false,
                          \ 'python': v:true,
                          \ 'markdown': v:true,
                          \ 'haskell': v:true,
                          \ }
                    let g:copilot_node_command = "${pkgs.nodejs_latest}/bin/node"
                    let g:copilot_proxy_strict_ssl = v:true
                  '';
                }
              ];
            };
            home.packages = with pkgs; [ neovide ];
          })
        ];
      })
  ];
  config = { programs.neovim.enable = true; };
}
