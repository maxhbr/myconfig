# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
let
  inco = with pkgs; writeScriptBin "inco.sh" ''
#!${stdenv.shell}
set -e
postfix=$(date +%s | sha256sum | base64 | head -c 32 ; echo)
mkdir -p "/tmp/incoChrome_$postfix"
${chromium}/bin/chromium --incognito \
    --user-data-dir="/tmp/incoChrome_$postfix" \
    $@ &disown
  '';
  pipechrome = with pkgs; writeScriptBin "pipechrome" ''
#!${stdenv.shell}
${chromium}/bin/chromium "data:text/html;base64,$(base64 -w 0 <&0)" &> /dev/null
  '';
  pipefox = with pkgs; writeScriptBin "pipefox" ''
#!${stdenv.shell}
${unstable.firefox}/bin/firefox "data:text/html;base64,$(base64 -w 0 <&0)" &> /dev/null
  '';
  mkscreenshot = with pkgs; writeScriptBin "mkscreenshot.sh" ''
#!${stdenv.shell}
set -e
output_dir="$HOME/_screenshots"
old_dir="$output_dir/_old"
output="$output_dir/$(date +%Y-%m-%d_%H:%M:%S).png"
mkdir -p "$output_dir"
mkdir -p "$old_dir"

echo "## clean up old screenshots ..."
find "$output_dir" -maxdepth 1 -mtime +10 -type f -print -exec mv {} "$old_dir" \;

echo "## take screenshot $output ..."
${imagemagick}/bin/import "$output"
  '';
in {
  imports = [
    ./my-wallpapers
  ];
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        # misc
        libnotify # xfce.xfce4notifyd # notify-osd

        networkmanager_dmenu

        # gui applications
        mupdf zathura llpp
        xarchiver
        feh imagemagick mkscreenshot # scrot
        mplayer
        chromium inco pipechrome
        unstable.firefox pipefox
        qutebrowser
        google-chrome # for streaming and music
        # spellchecking
        aspell aspellDicts.de aspellDicts.en
      ];
    };

    environment = {
      variables = {
        BROWSER = "${pkgs.chromium}/bin/chromium-browser";
      };
      interactiveShellInit = ''
        alias file-roller='${pkgs.xarchiver}/bin/xarchiver'
      '';
    };

    programs.light.enable = true;
    services.actkbd = {
      enable = true;
      bindings = [
        { keys = [ 224 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/light -U 10"; }
        { keys = [ 225 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/light -A 10"; }
      ];
    };

    services.printing = {
      enable = true;
      drivers = with pkgs; [ gutenprint hplip ];
      # add hp-printer with:
      # $ nix run nixpkgs.hplipWithPlugin -c sudo hp-setup
    };

    fonts = {
      enableFontDir = true;
      enableGhostscriptFonts = true;
      fonts = with pkgs; [
        dejavu_fonts
        corefonts
        inconsolata
      ];
    };
  };
}
