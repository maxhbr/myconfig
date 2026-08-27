{
  pkgs,
  config,
  lib,
  ...
}:
{
  config = lib.mkIf config.myconfig.desktop.enable {
    myconfig.persistence.directories = [
      ".mozilla"
      ".config/mozilla"
    ];
    myconfig.desktop.wayland.launcherCommands = [ "firefox" ];
    home-manager.sharedModules = [
      (
        { config, pkgs, ... }:
        let
          pipefox =
            with pkgs;
            writeShellScriptBin "pipefox" ''
              ${config.programs.firefox.package}/bin/firefox "data:text/html;base64,$(base64 -w 0 <&0)" &> /dev/null
            '';
        in
        {
          config = {
            home.packages = [ pipefox ];
            programs.firefox = {
              enable = lib.mkDefault true;
              configPath = "${config.xdg.configHome}/mozilla/firefox";
              profiles."0" = {
                id = 0;
                isDefault = true;
                name = "0";
                # settings = {
                #   # "browser.startup.homepage" = "https://nixos.org";
                #   "browser.tabs.splitView.enabled" = true;
                #   "browser.sessionstore.warnOnQuit" = true;
                #   "browser.search.region" = "DE";
                #   "browser.toolbars.bookmarks.2h2020" = true;
                #   "browser.toolbars.bookmarks.visibility" = "never";
                #   # "browser.search.isUS" = false;
                #   # "distribution.searchplugins.defaultLocale" = "en-GB";
                #   # "general.useragent.locale" = "en-GB";
                #   # "browser.bookmarks.showMobileBookmarks" = true;
                # };
                extensions.packages = (
                  with pkgs.nur.repos.rycee.firefox-addons;
                  [
                    # see: https://github.com/nix-community/nur-combined/blob/master/repos/rycee/pkgs/firefox-addons/generated-firefox-addons.nix
                    gopass-bridge
                    # https-everywhere
                    link-cleaner
                    privacy-badger
                    # tree-style-tab: upstream rycee/NUR pin (4.4.1 /
                    # AMO file 4983025) 404s because AMO rotated the file id.
                    # Override the fetch pin until nur-expressions regenerates:
                    # https://gitlab.com/rycee/nur-expressions
                    (tree-style-tab.override {
                      version = "4.4.3";
                      url = "https://addons.mozilla.org/firefox/downloads/file/4985978/tree_style_tab-4.4.3.xpi";
                      sha256 = "5d4d2a7ec7e3027d8e8e4e0b3d8a0355070267fbda8c927dfaeb0fd3b7cb09c8";
                    })
                    multi-account-containers
                    ublock-origin
                    vimium
                    sidebery
                  ]
                );
              };
            };
            xdg.mimeApps = {
              defaultApplications."x-scheme-handler/http" = [
                "firefox.desktop"
                "chromium.desktop"
                "qutebrowser.desktop"
              ];
              defaultApplications."x-scheme-handler/https" = [
                "firefox.desktop"
                "chromium.desktop"
                "qutebrowser.desktop"
              ];
              defaultApplications."text/html" = [ "firefox.desktop" ];
              defaultApplications."x-scheme-handler/about" = [ "firefox.desktop" ];
              defaultApplications."x-scheme-handler/unknown" = [ "firefox.desktop" ];
            };
          };
        }
      )
      {
        config = {
          home.file = {
            ".mozilla/native-messaging-hosts/com.justwatch.gopass.json" = {
              text = ''
                {
                "name": "com.justwatch.gopass",
                "description": "Gopass wrapper to search and return passwords",
                "path": "${pkgs.gopassWrapper}/bin/gopass_wrapper.sh",
                "type": "stdio",
                "allowed_extensions": [
                "{eec37db0-22ad-4bf1-9068-5ae08df8c7e9}"
                ]
                }
              '';
            };
          };
        };
      }
    ];
  };
}
