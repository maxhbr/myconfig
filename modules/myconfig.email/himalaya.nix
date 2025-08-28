{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig;
in
{
  config = lib.mkIf cfg.email.enable {
    home-manager.sharedModules = [(
      { config, lib, pkgs, ... }:
      {
        config = lib.mkIf config.programs.himalaya.enable {
          programs.himalaya = {
            package = pkgs.himalaya.override {
              withFeatures = lib.optionals config.programs.notmuch.enable [ "notmuch" ];
            };
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
    )];
  };
}