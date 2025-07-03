{
  pkgs,
  lib,
  config,
  ...
}:
let
  baseExtensions = with pkgs.vscode-extensions; [
    dracula-theme.theme-dracula
    vscodevim.vim
    yzhang.markdown-all-in-one
    haskell.haskell
    bbenoist.nix
    # ms-python.python
    ms-azuretools.vscode-docker
    ms-vscode-remote.remote-ssh

    alefragnani.project-manager
    antyos.openscad
    arrterian.nix-env-selector
    bbenoist.nix
    bierner.github-markdown-preview
    bierner.markdown-checkbox
    bierner.markdown-emoji
    bierner.markdown-footnotes
    bierner.markdown-mermaid
    bierner.markdown-preview-github-styles
    # bierner.markdown-yaml-preamble
    # dcortes92.freemarker
    # docker.docker
    editorconfig.editorconfig
    # hoovercj.haskell-linter
    # jakebecker.elixir-ls
    # jamesottaway.nix-develop
    jebbs.plantuml
    jnoortheen.nix-ide
    justusadam.language-haskell
    # kdpixels.graphite
    mathiasfrohlich.kotlin
    mechatroner.rainbow-csv
    ms-ceintl.vscode-language-pack-de
    # ms-dotnettools.csdevkit
    ms-dotnettools.csharp
    ms-dotnettools.vscode-dotnet-runtime
    ms-vscode-remote.remote-containers
    ms-vscode-remote.remote-ssh-edit
    ms-vscode.cpptools
    ms-vscode.cpptools-extension-pack
    # ms-vscode.cpptools-themes
    ms-vscode.makefile-tools
    # ms-vscode.remote-explore
    # nordic-semiconductor.nrf-devicetree
    # pinage404.nix-extension-pack
    redhat.java
    rust-lang.rust-analyzer
    # spadin.zmk-tools
    streetsidesoftware.code-spell-checker
    streetsidesoftware.code-spell-checker-german
    tamasfe.even-better-toml
    # tootone.org-mode
    twxs.cmake
    # vscjava.vscode-gradle
    # vscjava.vscode-java-debug
    # vscjava.vscode-java-dependency
    # vscjava.vscode-java-pack
    # vscjava.vscode-java-test
    # vscjava.vscode-maven
    vue.volar
    xaver.clang-format
    # ziyasal.vscode-open-in-github
  ];
in
{
  config = lib.mkIf config.myconfig.desktop.enable {
    home-manager.sharedModules = [
      (
        { config, ... }:
        {
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
            profiles.default = {
              extensions =
                with pkgs.vscode-extensions;
                [
                  github.copilot
                  github.copilot-chat
                ]
                ++ baseExtensions;
            };
            profiles.continue = {
              extensions = with pkgs.vscode-extensions; [ continue.continue ] ++ baseExtensions;
            };
            profiles.roo = {
              extensions = with pkgs.vscode-extensions; [ rooveterinaryinc.roo-cline ] ++ baseExtensions;
            };
            profiles.cline = {
              extensions = with pkgs.vscode-extensions; [ saoudrizwan.claude-dev ] ++ baseExtensions;
              userSettings = {
                "files.autoSave" = "on";
                "files.trimTrailingWhitespace" = true;
                "files.insertFinalNewline" = true;
                "window.titleBarStyle" = "custom"; # https://github.com/microsoft/vscode/issues/184124
                "accessibility.dimUnfocused.enabled" = true;
                "[plaintext]"."files.insertFinalNewline" = false;
                "[nix]"."editor.tabSize" = 2;
                "[haskell]"."editor.tabSize" = 2;
                "editor.experimental.asyncTokenization" = true;
                "editor.experimental.asyncTokenizationVerification" = true;
                "editor.inlineSuggest.enabled" = true;
                "github.copilot.enable" = {
                  "*" = false;
                };
                "github.copilot.advanced" = {
                  "enabled" = false;
                };
                "cline.chromeExecutablePath" = "${config.programs.chromium.package}/bin/chromium";
                "cline.disableBrowserTool" = false;
                "cline.enableCheckpoints" = true;
                "cline.mcpMarketplace.enabled" = true;
                "cline.modelSettings.o3Mini.reasoningEffort" = "medium";
                "cline.preferredLanguage" = "English";
                # "cline.vsCodeLmModelSelector" = {};
              };
            };
          };

          xdg.configFile = lib.mkIf config.myconfig.desktop.wayland.enable {
            # --enable-ozone
            # --enable-features=UseOzonePlatform
            "code-flags.conf".text = ''
              --ozone-platform=wayland
              --ozone-platform-hint=auto
              --enable-features=WaylandWindowDecorations
            '';
          };
          home.packages = lib.attrValues (
            lib.mapAttrs (
              name: profile:
              pkgs.writeShellScriptBin "code-${name}" ''
                ${config.programs.vscode.package}/bin/code --profile ${name} "$@"
              ''
            ) (lib.filterAttrs (name: profile: name != "default") config.programs.vscode.profiles)
          );
        }
      )
    ];
  };
}
