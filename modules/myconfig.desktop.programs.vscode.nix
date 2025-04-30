{ pkgs, lib, config, ... }: let
  baseProfile = {
    extensions = with pkgs.vscode-extensions; [
      dracula-theme.theme-dracula
      vscodevim.vim
      yzhang.markdown-all-in-one
      haskell.haskell
      bbenoist.nix
      # ms-python.python
      ms-azuretools.vscode-docker
      ms-vscode-remote.remote-ssh
    ];
  };
in {
  config = lib.mkIf config.myconfig.desktop.enable {
    home-manager.sharedModules = [
      ({ config, ... }: {
        programs.vscode = {
          enable = lib.mkDefault true;
          # package = pkgs.vscode-with-extensions;
          # userSettings = {
          #   "files.autoSave" = "on";
          #   "files.trimTrailingWhitespace" = true;
          #   "files.insertFinalNewline" = true;
          #   "window.titleBarStyle" =
          #     "custom"; # https://github.com/microsoft/vscode/issues/184124
          #   "accessibility.dimUnfocused.enabled" = true;
          #   "[plaintext]"."files.insertFinalNewline" = false;
          #   "[nix]"."editor.tabSize" = 2;
          #   "[haskell]"."editor.tabSize" = 2;
          #   "editor.experimental.asyncTokenization" = true;
          #   "editor.experimental.asyncTokenizationVerification" = true;
          #   "editor.inlineSuggest.enabled" = true;
          #   "github.copilot.enable" = {
          #     "*" = true;
          #     "plaintext" = false;
          #     # "yaml" = false;
          #     # "markdown" = true;
          #     # "javascript" = true;
          #     # "python" = true;
          #   };
          #   "github.copilot.advanced" = { "enabled" = true; };
          # };
          profiles.default = baseProfile; 
          profiles.continue = lib.recursiveUpdate baseProfile {
            extensions = with pkgs.vscode-extensions; [
              continue.continue
            ];
          };
        };
        myconfig.desktop.wayland.wrappedElectronPackages = [{
          pkg = config.programs.vscode.package;
          executable = "code";
          enabled = config.programs.vscode.enable;
        }];
        xdg.configFile = {
          # --enable-ozone
          # --enable-features=UseOzonePlatform
          "code-flags.conf".text = ''
            --ozone-platform=wayland
            --ozone-platform-hint=auto
            --enable-features=WaylandWindowDecorations
          '';
        };
      })
    ];
  };
}
