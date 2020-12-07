{ config, lib, pkgs, ... }:
let
  inco = with pkgs;
    writeShellScriptBin "inco.sh" ''
      set -e
      postfix=$(date +%s | sha256sum | base64 | head -c 32 ; echo)
      mkdir -p "/tmp/incoChrome_$postfix"
      ${chromium}/bin/chromium --incognito \
          --user-data-dir="/tmp/incoChrome_$postfix" \
          $@ &disown
        '';
  pipechrome = with pkgs;
    writeShellScriptBin "pipechrome" ''
      ${chromium}/bin/chromium "data:text/html;base64,$(base64 -w 0 <&0)" &> /dev/null
        '';
  # # see:
  # # - https://github.com/NixOS/nixpkgs/issues/3107
  # # - https://productforums.google.com/forum/#!msg/chromecast/G3E2ENn-YZI/s7Xoz6ICCwAJ
  # allowChromecast =
  #   "sudo ${pkgs.iptables}/bin/iptables -I INPUT -p udp -m udp -s 192.168.0.0/16 --match multiport --dports 1900,5353 -j ACCEPT";
in {
  config = (lib.mkIf config.programs.chromium.enable {
    home.packages = [inco pipechrome];
    programs.chromium = {
      package = pkgs.chromium.override {
          commandLineArgs = "--load-media-router-component-extension=1";
        };
      extensions = [

      ];
    };
  });
}
