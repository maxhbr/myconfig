{ config, lib, pkgs, ... }:
let
  user = config.myconfig.user;
  chromium = pkgs.chromium.override {
    commandLineArgs = "--load-media-router-component-extension=1";
  };
  inco = pkgs.writeShellScriptBin "inco.sh" ''
      set -e
      postfix=$(date +%s | sha256sum | base64 | head -c 32 ; echo)
      mkdir -p "/tmp/incoChrome_$postfix"
      ${pkgs.firejail}/bin/firejail --private --dns=8.8.8.8 \
          -c \
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
  # allowChromecast =
  #   "sudo ${pkgs.iptables}/bin/iptables -I INPUT -p udp -m udp -s 192.168.0.0/16 --match multiport --dports 1900,5353 -j ACCEPT";
in {
  config = (lib.mkIf config.services.xserver.enable {
    services.avahi.enable = true; # https://github.com/NixOS/nixpkgs/issues/49630
    home-manager.users."${user}" = {
      home.packages = [inco pipechrome];
      programs.chromium = {
        enable = true;
        package = chromium;
        extensions = [

        ];
      };
    };
  });
}
