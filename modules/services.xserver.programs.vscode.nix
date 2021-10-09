{pkgs, lib, config, ...}:
{
  config = (lib.mkIf config.services.xserver.enable {
    home-manager.sharedModules = [
      ({ config, ... }:
      {
        programms.vscode = {
          enable = true;
          package = pkgs.vscode-with-extensions;
          extensions = with pkgs.vscode-extensions; [
            dracula-theme.theme-dracula
            vscodevim.vim
            yzhang.markdown-all-in-one
          ];
        };
      })
    ];
  });
}
