{ pkgs, config, lib, ... }:
let
  pipefox = with pkgs;
    writeShellScriptBin "pipefox" ''
      ${unstable.firefox}/bin/firefox "data:text/html;base64,$(base64 -w 0 <&0)" &> /dev/null
        '';
in {
  imports = [({
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
  })];
  config = (lib.mkIf config.programs.firefox.enable {
    home.packages = with pkgs; [ pipefox ];
    programs.firefox = {
      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        # see: https://github.com/nix-community/nur-combined/blob/master/repos/rycee/pkgs/firefox-addons/generated-firefox-addons.nix
        gopass-bridge
        https-everywhere
        link-cleaner
        privacy-badger
        tree-style-tab
        ublock-origin
        vimium
      ];
      profiles."0" = {
        id = 0;
        isDefault = true;
        name = "0";
        # settings = {
        #   # "browser.startup.homepage" = "https://nixos.org";
        #   "browser.sessionstore.warnOnQuit"	= true;
        #   "browser.search.region" = "DE";
        #   "browser.toolbars.bookmarks.2h2020" = true;
        #   "browser.toolbars.bookmarks.visibility" = "never";
        #   # "browser.search.isUS" = false;
        #   # "distribution.searchplugins.defaultLocale" = "en-GB";
        #   # "general.useragent.locale" = "en-GB";
        #   # "browser.bookmarks.showMobileBookmarks" = true;
        # };
      };
    };
    xdg.mimeApps = {
      defaultApplications."x-scheme-handler/http" =
        [ "firefox.desktop" "chromium.desktop" "qutebrowser.desktop" ];
      defaultApplications."x-scheme-handler/https" =
        [ "firefox.desktop" "chromium.desktop" "qutebrowser.desktop" ];
    };
  });
}
