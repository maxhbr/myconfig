{ pkgs, lib, config, ... }: {
  config = lib.mkIf config.myconfig.desktop.enable {
    home-manager.sharedModules = [
      ({ config, ... }: {
        programs.vscode = {
          enable = lib.mkDefault true;
          # package = pkgs.vscode-with-extensions;
          userSettings = {
            "window.titleBarStyle" = "custom"; # https://github.com/microsoft/vscode/issues/184124
            "[nix]"."editor.tabSize" = 2;
          };
          extensions = with pkgs.vscode-extensions; [
            dracula-theme.theme-dracula
            vscodevim.vim
            yzhang.markdown-all-in-one
            haskell.haskell
            bbenoist.nix
            ms-python.python
            ms-azuretools.vscode-docker
            ms-vscode-remote.remote-ssh
          ];
        };
        xdg.configFile = {
# --enable-ozone
# --enable-features=UseOzonePlatform
          "code-flags.conf".text = ''
--ozone-platform=wayland
'';
        };
      })
    ];
  };
}
