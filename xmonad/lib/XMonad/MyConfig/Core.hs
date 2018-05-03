-- Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
{-# LANGUAGE CPP #-}
module XMonad.MyConfig.Core
    ( coreConfig
    , applyMyRestartKBs
    ) where

import           Data.Foldable ( foldMap )
import           System.Exit ( exitSuccess )
import           XMonad
import           Graphics.X11.ExtraTypes.XF86()

--------------------------------------------------------------------------------
-- Actions
import           XMonad.Actions.GridSelect
import           XMonad.Actions.WindowGo ( raiseNext )

--------------------------------------------------------------------------------
-- Hooks
import           XMonad.Hooks.ManageHelpers ( doCenterFloat )
import           XMonad.Hooks.SetWMName ( setWMName )

--------------------------------------------------------------------------------
-- Utils
import           XMonad.Util.Run ( runProcessWithInput )

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

coreConfig =
  def { terminal           = terminalCMD
      , borderWidth        = 3
      , modMask            = mod1Mask --  mod4Mask
      , normalBorderColor  = normalcolor
      , focusedBorderColor = maincolor
      , keys               = myKeys
      , layoutHook         = myLayout
      , manageHook         = myManageHook
      , startupHook        = myStartupHook
      }

applyMyRestartKBs executablePath =
  applyMyKBs [ ((m__, xK_q     ), spawn (executablePath ++ " --restart"))
             , ((ms_, xK_q     ), spawn (executablePath ++ " --recompile && sleep 0.1 && " ++ executablePath ++ " --restart")) ]

------------------------------------------------------------------------
-- Window rules:
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = let
  baseHooks = (foldMap (\(a,cs) -> map (\c -> className =? c --> a) cs)
                             [ (doCenterFloat, ["Xmessage"
                                               ,"qemu","qemu-system-x86_64"
                                               ,"feh"
                                               ,"Zenity"
                                               ,"pinentry","Pinentry"
                                               ,"pavucontrol","Pavucontrol"
                                               ,"zoom"])
                             , (doFloat, ["MPlayer"
                                         ,"Onboard"])
                             , (doShift "web", ["Firefox"
                                               ,"Chromium","chromium-browser"])
                             , (doShift "10", ["franz","Franz"])
                             , (doShift "vbox", ["Virtualbox","VirtualBox"])
                             , (doShift "media", ["Steam"])
                             , (doIgnore, ["desktop_window"
                                          ,"kdesktop"]) ])
  -- see:
  -- - https://www.reddit.com/r/xmonad/comments/78uq0p/how_do_you_deal_with_intellij_idea_completion/?st=jgdc0si0&sh=7d79c956
  -- - https://youtrack.jetbrains.com/issue/IDEA-112015#comment=27-2498787
  ideaPopupHook = [ appName =? "sun-awt-X11-XWindowPeer" <&&> className =? "jetbrains-idea" --> doIgnore ]
  in composeAll (baseHooks ++ ideaPopupHook)

------------------------------------------------------------------------
-- Startup hook:
myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"
  spawn ("pkill unclutter; " ++ unclutterCMD)
  spawn (xsetCMD ++ " s 900")

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
    basicKBs =
      [ ((ms_            , xK_Return), spawn $ XMonad.terminal conf)
      , ((msc, xK_Return), spawn (terminalServerCMD ++ " &"))
#if 1
      , ((m4m, xK_Return), windows W.swapMaster)
#else
      , ((m__, xK_Return), windows W.swapMaster)
#endif
      , ((msc, xK_q     ), io exitSuccess)
      , ((m__, xK_p     ), spawn myLauncherCMD)
      -- , ((m__, xK_x     ), shellPrompt defaultXPConfig)
      , ((ms_, xK_space ), setLayout $ layoutHook conf) -- reset layout


      , ((ms_, xK_c     ), kill)

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
      , ((m__, xK_z      ), spawn (myautosetupCMD ++ " --onlyIfChanged"))
      , ((ms_, xK_z      ), spawn myautosetupCMD)
      , ((msc, xK_z      ), spawn (myautosetupCMD ++ " --rotate=left --primOutNr=1"))
#if 1
      , ((ms_, xK_y      ), spawn (xsetCMD ++ " s activate")) -- screenlocker
#else
      , ((ms_, xK_y      ), spawn "slimlock") -- screenlocker
#endif

      -- invert Colors (does not work with retdshift)
      , ((m__,  xK_Home   ), spawn invertColorsCMD)

      , ((m__,  xK_Print  ), spawn screenshotCMD)

      -- keyboard layouts
      , ((m__,  xK_F2     ), spawn (fehCMD ++ " " ++ pathToXmonadShare ++ "neo_Ebenen_1_2_3_4.png"))
      , ((m__,  xK_F3     ), spawn (fehCMD ++ " " ++ pathToXmonadShare ++ "neo_Ebenen_1_2_5_6.png"))]

backlightControlKBs, volumeControlKBs :: [((KeyMask -> KeyMask, KeySym), X ())]
backlightControlKBs = [((m__, xK_F1), spawnSelected def [ "xbacklight =50"
                                                        , "xbacklight =25"
                                                        , "xbacklight +10"
                                                        , "xbacklight =75"
                                                        , "xbacklight -10"
                                                        , "xbacklight =10"
                                                        , "xbacklight =5"
                                                        , "xbacklight +1"
                                                        , "xbacklight =3"
                                                        , "xbacklight =100"
                                                        , "xbacklight =1"
                                                        , "xbacklight -1"
                                                        , "xbacklight =0" ])]

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

