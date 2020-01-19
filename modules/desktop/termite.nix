{ pkgs, ... }:
{
  config = {
    home-manager.users.mhuber = {
      programs.termite = {
        enable = true;

        allowBold = true;
        audibleBell = false;
        clickableUrl = true;
        dynamicTitle = true;
        fullscreen = false;
        mouseAutohide = false;
        scrollOnOutput = true; # TODO
        scrollOnKeystroke = true; # TODO
        searchWrap = false;
        urgentOnBell = true;
        font = "inconsolata 17";
        scrollbackLines = 10000;
        browser = "${pkgs.xdg_utils}/xdg-open";
        cursorBlink = "system";
        cursorShape = "ibeam"; # type = types.nullOr (types.enum [ "block" "underline" "ibeam" ]);
        scrollbar = "off";

        backgroundColor = "rgba(249, 249, 249, 1)";
        cursorColor = "#d0d0d0";
        cursorForegroundColor = null;
        foregroundColor = "#383a42";
        foregroundBoldColor = null;
        highlightColor = null;
        hintsActiveBackgroundColor = null;
        hintsActiveForegroundColor = null;
        hintsBackgroundColor = null;
        hintsForegroundColor = null;
        hintsBorderColor = null;
        hintsBorderWidth = null;
        hintsFont = null;
        hintsPadding = null;
        hintsRoundness = null;
        optionsExtra = "";
        colorsExtra = ''
          color0  = #000000
          color1  = #E45649
          color2  = #50A14F
          color3  = #986801
          color4  = #4078F2
          color5  = #A626A4
          color6  = #0184BC
          color7  = #A0A1A7
          color8  = #5c6370
          color9  = #e06c75
          color10 = #50A14F
          color11 = #986801
          color12 = #4078F2
          color13 = #A626A4
          color14 = #0184BC
          color15 = #ffffff
        '';
      };
    };
  };
}
