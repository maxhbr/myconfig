# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
let
  user = config.myconfig.user;
  myStopScreensaver = with pkgs;
    writeScriptBin "myStopScreensaver" ''
      #!${stdenv.shell}
      printf "run: "
      while true; do
          printf "."
          sleep $((60 * 4))
          ${xdotool}/bin/xdotool key shift
      done
    '';
  myInvert = with pkgs;
    writeScriptBin "myInvert" ''
      #!${stdenv.shell}
      ${systemd}/bin/systemctl --user stop redshift
      ${xrandr-invert-colors}/bin/xrandr-invert-colors
    '';
  mySetBrightness = with pkgs;
    writeScriptBin "mySetBrightness" ''
      #!${stdenv.shell}
      ${systemd}/bin/systemctl --user stop redshift
      ${xorg.xrandr}/bin/xrandr --output DP-2 --brightness $${1:-1}
    '';
  myXsecurelock = with pkgs;
    writeScriptBin "myXsecurelock" ''
      #!${stdenv.shell}
      export XSECURELOCK_SAVER=saver_blank
      export XSECURELOCK_PASSWORD_PROMPT=time
      ${xsecurelock}/bin/xsecurelock
    '';

in {
  imports = [ ../desktop.common ./big-cursor.nix ./autorandr.nix ./xclip.nix ];

  config = {
    home-manager.users."${user}" = {
      home.packages = with pkgs; [
        arandr
        xlibs.xmodmap
        xlibs.xset
        xlibs.setxkbmap
        xorg.xkill
        xorg.xmessage
        xclip
        xdotool
        myStopScreensaver
        xrandr-invert-colors
        myInvert
        mySetBrightness
      ];
      home.file = {
        ".fontconfig/fonts.conf".source = ./fontconfig/fonts.conf;
        ".config/zathura/zathurarc".source = ./config/zathura/zathurarc;
      };
      xresources.extraConfig = builtins.readFile ./Xresources;
    };

    environment.interactiveShellInit = ''
      xclipToX() {
        ${pkgs.xclip}/bin/xclip <(${pkgs.xclip}/bin/xclip -selection c -o)
      }

      xclipToCtrl() {
        ${pkgs.xclip}/bin/xclip -selection c <(${pkgs.xclip}/bin/xclip -o)
      }
    '';

    programs.xss-lock = {
      enable = true;
      # lockerCommand = "${pkgs.my-wallpapers}/bin/myScreenLock";
      lockerCommand = "${myXsecurelock}/bin/myXsecurelock";
      extraOptions =
        [ "-n" "${myXsecurelock}/libexec/xsecurelock/dimmer" "-l" ];
    };
    services = {
      xserver = {
        enable = true;
        autorun = true;
        layout = "de";
        xkbVariant = "neo";
        xkbOptions = "altwin:swap_alt_win";
        enableCtrlAltBackspace = true;

        displayManager = {
          lightdm = {
            enable = true;
            background = "${pkgs.my-wallpapers}/share/romben3.png";
          };
          sessionCommands = ''
            if test -e $HOME/.Xresources; then
              ${pkgs.xorg.xrdb}/bin/xrdb -merge $HOME/.Xresources &disown
            fi
            ${pkgs.my-wallpapers}/bin/myRandomBackground &disown
          '';
        };
      };

      cron = {
        enable = true;
        systemCronJobs = [
          "*/10 * * * *  ${user} ${pkgs.my-wallpapers}/bin/myRandomBackground >> /tmp/cronout 2>&1"
        ];
      };

      redshift = {
        enable = true;
        # temperature.day = 5500;
        # temperature.night = 3500;
        # brightness.day = "0.8";
        # brightness.night = "0.5";
      };
    };
  };
}
