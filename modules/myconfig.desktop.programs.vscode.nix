{ pkgs, lib, config, ... }: {
  config = lib.mkIf config.myconfig.desktop.enable {
    home-manager.sharedModules = [
      ({ config, ... }: {
        programs.vscode = {
          enable = lib.mkDefault true;
          # package = pkgs.vscode-with-extensions;
          userSettings = {
            "files.autoSave" = "on";
            "files.trimTrailingWhitespace" = true;
            "files.insertFinalNewline" = true;
            "window.titleBarStyle" = "custom"; # https://github.com/microsoft/vscode/issues/184124
            "[plaintext]"."files.insertFinalNewline" = false;
            "[nix]"."editor.tabSize" = 2;
            "[haskell]"."editor.tabSize" = 4;
            "editor.experimental.asyncTokenization" = true;
            "editor.experimental.asyncTokenizationVerification" = true;
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
