# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
let
  urxvt = pkgs.rxvt_unicode_with-plugins;
in {
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        urxvt rxvt_unicode.terminfo
      ];
      xresources.properties = {
        "URxvt*saveLines" = "12000";
        "URxvt*reverseVideo" = "false";
        "URxvt*loginShell" = "true";
        "URxvt.urgentOnBell" = "true";
        "URxvt*termName" = "rxvt-256color";

        "URxvt*scrollBar"       = "false";
        "URxvt*scrollBar_right" = "false";
        "URxvt*scrollstyle"     = "plain";
        "URxvt.internalBorder" = "5";
        "URxvt.externalBorder" = "5";

        "URxvt.font"           = "xft:inconsolata:pixelsize=17:antialias=true:hinting=true,             xft:DejaVu Sans Mono:pixelsize=15:antialias=true:hinting=true";
        "URxvt.boldFont"       = "xft:inconsolata:bold:pixelsize=17:antialias=true:hinting=true,        xft:DejaVu Sans Mono:bold:pixelsize=14:antialias=true:hinting=true";
        "URxvt.italicFont"     = "xft:inconsolata:italic:pixelsize=17:antialias=true:hinting=true,      xft:DejaVu Sans Mono:italic:pixelsize=15:antialias=true:hinting=true";
        "URxvt.bolditalicFont" = "xft:inconsolata:bold:italic:pixelsize=17:antialias=true:hinting=true, xft:DejaVu Sans Mono:bold:italic:pixelsize=15:antialias=true:hinting=true";

        "urxvt.perl-lib" = "${urxvt}/lib/perl5/";
        "urxvt.perl-ext-common" = "default,matcher,tabbed,font-size,clipboard,url-select,keyboard-select";
        "urxvt.matcher.button" = "1";

        "URxvt.tabbed.autohide" = "yes";
        "URxvt.tabbed.tabbar-fg" = "8";
        "URxvt.tabbed.tabbar-bg" = "15";
        "URxvt.tabbed.tab-fg" = "2";
        "URxvt.tabbed.tab-bg" = "15";

        "URxvt.keysym.C-Up" = "perl:font-size:increase";
        "URxvt.keysym.C-Down" = "perl:font-size:decrease";
        "URxvt.keysym.C-S-Up" = "perl:font-size:incglobal";
        "URxvt.keysym.C-S-Down" = "perl:font-size:decglobal";

        "URxvt.urlLauncher" = "${pkgs.xdg_utils}/bin/xdg-open";
        "URxvt.url-select.launcher" = "${pkgs.xdg_utils}/bin/xdg-open";
        "URxvt.url-select.underline" = "true";
        "URxvt.keysym.M-u" = "perl:url-select:select_next";
        "URxvt.keysym.M-Escape" = "perl:keyboard-select:activate";
        "URxvt.keysym.M-s" = "perl:keyboard-select:search";

        "URxvt.depth" = "32";
      } // {
        "URxvt*transparent" = "true";
        "URxvt*fading" = "30";
        "URxvt.shading" = "195";

        "urxvt*background" = "#f9f9f9";
        "urxvt*foreground" = "#383a42";
        "urxvt*cursorColor" = "#d0d0d0";
        "urxvt*color0"  = "#000000";
        "urxvt*color1"  = "#E45649";
        "urxvt*color2"  = "#50A14F";
        "urxvt*color3"  = "#986801";
        "urxvt*color4"  = "#4078F2";
        "urxvt*color5"  = "#A626A4";
        "urxvt*color6"  = "#0184BC";
        "urxvt*color7"  = "#A0A1A7";
        "urxvt*color8"  = "#5c6370";
        "urxvt*color9"  = "#e06c75";
        "urxvt*color10" = "#50A14F";
        "urxvt*color11" = "#986801";
        "urxvt*color12" = "#4078F2";
        "urxvt*color13" = "#A626A4";
        "urxvt*color14" = "#0184BC";
        "urxvt*color15" = "#ffffff";
      };
    };
    services = {
      urxvtd = {
        enable = true;
        package = urxvt;
      };
    };
  };
}
