{ config, lib, pkgs, ... }:
let
  chromium = pkgs.chromium.override {
    commandLineArgs = "--load-media-router-component-extension=1";
  };
  inco = pkgs.writeShellScriptBin "inco.sh" ''
    set -e
    if ${pkgs.networkmanager}/bin/nmcli connection show --active | grep " tun"; then
      echo "found vpn, exit"
      ${pkgs.libnotify}/bin/notify-send --expire-time=1000 --urgency="critical" --transient "!inco.sh" "found vpn"
      exit 3
    fi
    postfix=$(date +%s | sha256sum | base64 | head -c 32 ; echo)
    mkdir -p "/tmp/incoChrome_$postfix"
    ${chromium}/bin/chromium --incognito \
        --user-data-dir="/tmp/incoChrome_$postfix" \
        $@ &disown
  '';
  pipechrome = pkgs.writeShellScriptBin "pipechrome" ''
    ${chromium}/bin/chromium "data:text/html;base64,$(base64 -w 0 <&0)" &> /dev/null
  '';
  # # see:
  # # - https://github.com/NixOS/nixpkgs/issues/3107
  # # - https://productforums.google.com/forum/#!msg/chromecast/G3E2ENn-YZI/s7Xoz6ICCwAJ
  allowChromecast = pkgs.writeShellScriptBin "allowChromecast"
    "sudo ${pkgs.iptables}/bin/iptables -I INPUT -p udp -m udp -s 192.168.0.0/16 --match multiport --dports 1900,5353 -j ACCEPT";
in {
  config = lib.mkIf config.myconfig.desktop.enable {
    services.avahi.enable =
      true; # https://github.com/NixOS/nixpkgs/issues/49630
    home-manager.sharedModules = [({config, ...}: {
      home.packages = [ inco pipechrome allowChromecast ];
      home.file = {
        ".config/chromium/NativeMessagingHosts/com.justwatch.gopass.json" = {
          text = ''
            {
                "name": "com.justwatch.gopass",
                "description": "Gopass wrapper to search and return passwords",
                "path": "${pkgs.gopassWrapper}/bin/gopass_wrapper.sh",
                "type": "stdio",
                "allowed_origins": [
                    "chrome-extension://kkhfnlkhiapbiehimabddjbimfaijdhk/"
                ]
            }
          '';
        };
      };
      programs.chromium = {
        enable = lib.mkDefault true;
        package = chromium;
        extensions = [

        ];
      };
      myconfig.desktop.wayland.wrappedElectronPackages = [
        {
          pkg = config.programs.chromium.package;
          executable = "chromium";
          enabled = config.programs.chromium.enabled;
        }
      ];
    })];
  };
}
