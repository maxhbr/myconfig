-- Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
module XMonad.MyConfig.Variables
    where

pathToXmobarConfig = "~/.xmonad/xmobarrc"
pathToXmobarMinConfig = "~/.xmonad/xmobarrc.minimal"
pathToXmonadBins   = "~/.xmonad/bin/"
pathToXmonadShare  = "~/.xmonad/share/"
pathToMyconfigBins = ""
pathToMyXmonadBin  = ""

xmobarCMD          = "xmobar"
terminalCMD        = "urxvtc"
terminalServerCMD  = "urxvtd -q -f -o"
bashCMD            = "bash"
zshCMD             = "zsh"
editorCMD          = "ec"
emacsCMD           = "emacs"
dmenuPathCMD       = "dmenu_path"
yeganeshCMD        = "yeganesh"
passmenuCMD        = "passmenu"
firefoxCMD         = "firefox"
browserCMD         = "chromium-browser"
findCursorCMD      = "find-cursor"
xdotoolCMD         = "xdotool"
synclientCMD       = "synclient"
xsetCMD            = "xset"
invertColorsCMD    = "xrandr-invert-colors"
-- or: "xcalib -i -a"
fehCMD             = "feh"
unclutterCMD       = "unclutter"
htopCMD            = "htop"
pavucontrolCMD     = "pavucontrol"
lightCMD           = "light"

myautosetupCMD     = pathToMyconfigBins ++ "myautosetup.pl"
screenshotCMD      = pathToMyconfigBins ++ "screenshot.sh"
batteryMonitorCMD  = pathToXmonadBins ++ "battery-monitor.sh"
-- or:
-- - "bash -c \"import -frame ~/screen_`date +%Y-%m-%d_%H-%M-%S`.png\"")
-- - "mkdir -p ~/_screenshots/ && scrot ~/_screenshots/screen_%Y-%m-%d_%H-%M-%S.png -d 1"
