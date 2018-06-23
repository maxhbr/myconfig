-- Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
{-# LANGUAGE CPP #-}
module XMonad.MyConfig.Core
    ( coreConfig
    , applyMyRestartKBs
    ) where

import           System.FilePath
import           System.Exit ( exitSuccess )
import           XMonad
import           XMonad.Operations (restart)
import           Graphics.X11.ExtraTypes.XF86( xF86XK_MonBrightnessUp, xF86XK_MonBrightnessDown )

--------------------------------------------------------------------------------
-- Actions
import           XMonad.Actions.GridSelect
import           XMonad.Actions.WindowGo ( raiseNext )

--------------------------------------------------------------------------------
-- Hooks
import           XMonad.Hooks.SetWMName ( setWMName )


--------------------------------------------------------------------------------
-- Utils
import           XMonad.Util.Run ( runProcessWithInput )
import           XMonad.Actions.WindowGo ( runOrRaiseNext )

--------------------------------------------------------------------------------
-- misc
import qualified Data.Map        as M
import qualified XMonad.StackSet as W

--------------------------------------------------------------------------------
-- MyConfig
import XMonad.MyConfig.Common
import XMonad.MyConfig.MyLayoutLayer ( myLayout )
import XMonad.MyConfig.Notify ( popupCurDesktop, myDefaultPopup )

normalcolor = "#333333" :: String
maincolor = "#ee9a00" :: String

myModMask = mod1Mask -- or: mod4Mask

coreConfig =
  def { terminal           = terminalCMD
      , modMask            = myModMask
      , borderWidth        = 3
      , normalBorderColor  = normalcolor
      , focusedBorderColor = maincolor
      , keys               = myKeys
      , layoutHook         = myLayout
      , startupHook        = myStartupHook
      }

applyMyRestartKBs executablePath =
  applyMyKBs [ ((m__, xK_q     ), restart executablePath True)
             , ((m_c, xK_q     ), spawn (executablePath ++ " --restart"))
             -- , ((ms_, xK_q     ), spawn (executablePath ++ " --recompile && sleep 0.1 && " ++ executablePath ++ " --restart"))
             ]

------------------------------------------------------------------------
-- Startup hook:
myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"
  spawn ("pkill unclutter; " ++ unclutterCMD)
  spawn (xsetCMD ++ " s 900")
  spawn batteryMonitorCMD

------------------------------------------------------------------------
-- Key bindings:
myKeys conf =
  M.fromList $
  mapToWithModM conf $
       basicKBs
    ++ raiseKBs
    ++ miscKBs
    ++ systemctlKBs
    ++ backlightControlKBs
    ++ volumeControlKBs
  where
    browserX     = runOrRaiseNext browserCMD (className =? "Firefox" <||> className =? "Firefox-bin" <||> className =?  "chromium-browser" <||> className =? "Chromium-browser")
    editorX      = runOrRaiseNext editorCMD (className =? "Emacs")
    pavucontrolX = runOrRaiseNext pavucontrolCMD (className =? "pavucontrol" <||> className =? "Pavucontrol")

    myLauncherCMD = let
      additionalPath = if isAbsolute dmenuPathCMD
                       then "PATH=$PATH:" ++ takeDirectory dmenuPathCMD
                       else ""
      in "x=$(" ++ additionalPath ++ " " ++ yeganeshCMD ++ " -x --) && exec $x"
    -- - "`dmenu_path | yeganesh`"

    basicKBs =
      [ ((ms_            , xK_Return), spawn $ XMonad.terminal conf)
      , ((msc, xK_Return), spawn (terminalServerCMD ++ " &"))
#if 1
      , ((m4m, xK_Return), windows W.swapMaster)
#else
      , ((m__, xK_Return), windows W.swapMaster)
#endif
      , ((ms_, xK_c     ), kill)
      , ((m__, xK_p     ), spawn myLauncherCMD)
      , ((ms_, xK_q     ), spawn (pathToXmonadBins ++ "stopWM.sh"))
      , ((msc, xK_q     ), io exitSuccess)
      -- , ((m__, xK_x     ), shellPrompt defaultXPConfig)
      , ((ms_, xK_space ), setLayout $ layoutHook conf) -- reset layout

      , ((m_c, xK_Return), spawn (terminalCMD ++ " -e " ++ zshCMD ++ " -c 'ssh vserver'"))
      , ((ms_, xK_p     ), spawn passmenuCMD)]
    systemctlKBs =
      map (\(k,v) -> (k, spawn $ "systemctl " ++ v))
        [ ((ms_, xK_F10), "suspend")
        , ((msc, xK_F11), "reboot")
        , ((msc, xK_F12), "poweroff")]
    raiseKBs = map (\(a,b) -> (a,b >> popupCurDesktop))
       [ ((m__, xK_i), browserX)
       , ((m__, 0xfc), editorX)
       , ((m__, 0xf6), raiseNext (className =? "jetbrains-phpstorm" <||> className =? "jetbrains-idea"))
       , ((const 0, 0x1008ffb2), pavucontrolX)]
    miscKBs =
      [ ((const 0,   0x1008ffa9), spawn (synclientCMD ++ " TouchpadOff=$(" ++ synclientCMD ++ " -l | grep -c 'TouchpadOff.*=.*0')"))
      , ((m__, xK_s      ), spawn findCursorCMD)
      , ((msc, xK_s      ), spawn (xdotoolCMD ++ " mousemove 0 0; " ++ synclientCMD ++ " TouchpadOff=$(" ++ synclientCMD ++ " -l | grep -c 'TouchpadOff.*=.*0')"))
      , ((ms_, xK_y      ), spawn (xsetCMD ++ " s activate")) -- screenlocker
      , ((m__, xK_Home   ), spawn invertColorsCMD)
      , ((m__, xK_Print  ), spawn screenshotCMD)

      -- keyboard layouts
      , ((m__, xK_F2     ), spawn (fehCMD ++ " " ++ pathToXmonadShare ++ "neo_Ebenen_1_2_3_4.png"))
      , ((m__, xK_F3     ), spawn (fehCMD ++ " " ++ pathToXmonadShare ++ "neo_Ebenen_1_2_5_6.png"))
      , ((m__, xK_z      ), spawn (myautosetupCMD ++ " --onlyIfChanged"))
      , ((ms_, xK_z      ), spawn myautosetupCMD)
      ]

backlightControlKBs, volumeControlKBs :: [((KeyMask -> KeyMask, KeySym), X ())]
backlightControlKBs = let
     xbacklightCMDs = [ "=50"
                      , "=25"
                      , "+10"
                      , "=75"
                      , "-10"
                      , "=10"
                      , "=5"
                      , "+1"
                      , "=3"
                      , "=100"
                      , "=1"
                      , "-1"
                      , "=0" ]
     xbacklightRunSelectedConf = map (\ s -> (s, spawn (xbacklightCMD ++ " " ++ s))) xbacklightCMDs
  in [ ((m__, xK_F1), runSelectedAction def xbacklightRunSelectedConf)
     , ((const 0, xF86XK_MonBrightnessUp)   , spawn (xbacklightCMD ++ " -inc 10%"))
     , ((const 0, xF86XK_MonBrightnessDown) , spawn (xbacklightCMD ++ " -dec 10%"))
     ]

volumeControlKBs =
#if 1
-- pulseaudio
  map (\(k,args) -> ((const 0, k)
                 , runProcessWithInput (pathToXmonadBins ++ "mypamixer.sh") args ""
                   >>= myDefaultPopup . ("V: " ++)
                 ))
    [ (0x1008ff12, ["mute"])
    , (0x1008ff11, ["-10%"])
    , (0x1008ff13, ["+10%"])]
#else
-- alsa
  map (\(k,v) -> ((const 0, k)
                 , runProcessWithInput (pathToXmonadBins ++ "myamixer.sh") v ""
                   >>= myDefaultPopup
                 ))
    [ (0x1008ff12, ["toggle"])
    , (0x1008ff11, ["6dB-"])
    , (0x1008ff13, ["unmute","3dB+"])]
#endif

