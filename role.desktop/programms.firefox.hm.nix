{pkgs, config, lib, ...}:
let
  pipefox = with pkgs;
    writeShellScriptBin "pipefox" ''
      ${unstable.firefox}/bin/firefox "data:text/html;base64,$(base64 -w 0 <&0)" &> /dev/null
        '';
in {
  imports = [
    (let
        gopassWrapper = with pkgs;
          writeShellScriptBin "gopass_wrapper.sh" ''
      if [ -f ~/.gpg-agent-info ] && [ -n "$(${procps}/bin/pgrep gpg-agent)" ]; then
        source ~/.gpg-agent-info
        export GPG_AGENT_INFO
      else
        eval $(${gnupg}/bin/gpg-agent --daemon)
      fi
      export GPG_TTY="$(tty)"

      exec ${gopass}/bin/gopass-jsonapi listen
    '';
      in {
        config = {
          home.file = {
            ".mozilla/native-messaging-hosts/com.justwatch.gopass.json" = {
              text = ''
            {
                "name": "com.justwatch.gopass",
                "description": "Gopass wrapper to search and return passwords",
                "path": "${gopassWrapper}/bin/gopass_wrapper.sh",
                "type": "stdio",
                "allowed_extensions": [
                    "{eec37db0-22ad-4bf1-9068-5ae08df8c7e9}"
                ]
            }
          '';
            };
            ".config/chromium/NativeMessagingHosts/com.justwatch.gopass.json" = {
              text = ''
            {
                "name": "com.justwatch.gopass",
                "description": "Gopass wrapper to search and return passwords",
                "path": "${gopassWrapper}/bin/gopass_wrapper.sh",
                "type": "stdio",
                "allowed_origins": [
                    "chrome-extension://kkhfnlkhiapbiehimabddjbimfaijdhk/"
                ]
            }
          '';
            };
          };
        };
      })
  ];
  home.packages = with pkgs; [
    pipefox
  ];
  programs.firefox = {
    enable = true;
    package = pkgs.unstable.firefox;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      # see: https://github.com/nix-community/nur-combined/blob/master/repos/rycee/pkgs/firefox-addons/generated-firefox-addons.nix
      https-everywhere
      privacy-badger
      link-cleaner
      gopass-bridge
    ];
    profiles."0" = {
      id = 0;
      isDefault = true;
      name = "0";
      settings = {
        # "browser.startup.homepage" = "https://nixos.org";
        "browser.search.region" = "DE";
        # "browser.search.isUS" = false;
        # "distribution.searchplugins.defaultLocale" = "en-GB";
        # "general.useragent.locale" = "en-GB";
        # "browser.bookmarks.showMobileBookmarks" = true;
      };
    };
  };
}
