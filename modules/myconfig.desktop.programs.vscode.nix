{ pkgs, lib, config, ... }: {
  config = lib.mkIf config.myconfig.desktop.enable {
    home-manager.sharedModules = [
      ({ config, ... }: {
        programs.vscode = {
          enable = lib.mkDefault true;
          # package = pkgs.vscode-with-extensions;
          extensions = with pkgs.vscode-extensions; [
            dracula-theme.theme-dracula
            vscodevim.vim
            yzhang.markdown-all-in-one
            haskell.haskell
          ];
        };
      })
    ];
  };
}
