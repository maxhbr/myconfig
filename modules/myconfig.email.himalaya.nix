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
  config = lib.mkIf (cfg.email.enable && (builtins.elem "himalaya" cfg.email.clients)) {
    home-manager.sharedModules = [
      (
        {
          config,
          lib,
          pkgs,
          ...
        }:
        {
          config = {
            programs.himalaya = {
              enable = true;
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
            home.packages = with pkgs; (lib.optionals (config.programs.neovide.enable) [
              (writeShellScriptBin "himalaya-neovide" ''
                exec ${config.programs.neovide.package}/bin/neovide +Himalaya
              '')
            ] ++ lib.optionals (config.programs.neovim.enable) [
              (writeShellScriptBin "himalaya-nvim" ''
                exec ${config.programs.neovim.package}/bin/nvim +Himalaya
              '')
            ]);
          };
        }
      )
    ];
  };
}
