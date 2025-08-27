{ config, lib, pkgs, ... }:
{
  config = lib.mkIf config.programs.meli.enable {
    programs.meli = {
      package = pkgs.nixos-2505.meli; # unstable (0.8.12) currently is broken
    };
    programs.neovim = {
      plugins = with pkgs.vimPlugins; [
        telescope-nvim
        {
          plugin = himalaya-vim;
          type = "lua";
          config = ''
            vim.g.himalaya_folder_picker = 'telescope'
          '';
        }
      ];
    };
  };
}
