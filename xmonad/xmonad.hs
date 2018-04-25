-- Copyright 2017-2018 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
-- ~/.xmonad/xmonad.hs
-- needs xorg-xmessage for error messages
--
-- xmonad-extras from cabal
--
-- used software
--  dmenu        to start software
--  dwb          fast browser
--  scrot        screenshot tool
--  imagemagic   screenshot tool
--  slim         screenlock tool
--  xss-lock     automatic locking
--  unclutter    to hide mouse pointer
--  urxvt        terminal
--  xcalib       invert colors
--  xmobar       bar
--  pass         password manager
{-# OPTIONS_GHC -W -fwarn-unused-imports -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}

------------------------------------------------------------------------
-- Imports:
import           Data.Foldable (foldMap)
import           Data.Ratio ((%))
import           System.Exit ( exitSuccess )
import           System.IO ( hPutStrLn )
import           XMonad
import           Graphics.X11.ExtraTypes.XF86()

--------------------------------------------------------------------------------
-- Prompt

--------------------------------------------------------------------------------
-- Hooks
import           XMonad.Hooks.DynamicLog ( dynamicLogWithPP
                                         , PP(..)
                                         , xmobarColor
                                         , wrap )
import           XMonad.Hooks.EwmhDesktops ( fullscreenEventHook )
import           XMonad.Hooks.ManageHelpers ( doCenterFloat )
import           XMonad.Hooks.SetWMName ( setWMName )

--------------------------------------------------------------------------------
-- Util
import           XMonad.Util.Run ( spawnPipe )
import           XMonad.Util.Types ( Direction2D(..) )

--------------------------------------------------------------------------------
-- Actions
import           XMonad.Actions.GridSelect

--------------------------------------------------------------------------------
-- misc
import qualified Data.Map                    as M
import qualified XMonad.StackSet             as W

--------------------------------------------------------------------------------
-- MyConfig
import XMonad.MyConfig
import XMonad.MyConfig.Utils
import XMonad.MyConfig.Common
import XMonad.MyConfig.Scratchpads
import XMonad.MyConfig.ToggleFollowFocus
import XMonad.MyConfig.Notify
import XMonad.MyConfig.MyLayoutLayer
import XMonad.MyConfig.MyMiscLayers

------------------------------------------------------------------------
-- Key bindings:
myKeys conf =
  M.fromList $
  mapToWithModM conf $
       basicKBs
    ++ miscKBs
    ++ systemctlKBs
  where
    basicKBs =
      [ ((ms_            , xK_Return), spawn $ XMonad.terminal conf)
      , ((msc, xK_Return), spawn "urxvtd -q -f -o &")
#if 1
      , ((m4m, xK_Return), windows W.swapMaster)
#else
      , ((m__, xK_Return), windows W.swapMaster)
#endif
      , ((m__, xK_q     ), spawn "xmonad --restart")
      , ((ms_, xK_q     ), spawn "xmonad --recompile && sleep 0.1 && xmonad --restart")
      , ((msc, xK_q     ), io exitSuccess)
      , ((m__, xK_p     ), spawn "`dmenu_path | yeganesh`")
      -- , ((m__, xK_x     ), shellPrompt defaultXPConfig)


      , ((ms_, xK_c     ), kill)

      , ((m__, xK_o     ), spawn "urxvtc -e bash -c 'EDITOR=vim ranger'")
      , ((m_c, xK_Return), spawn "urxvtc -e zsh -c 'ssh vserver'")
      , ((ms_, xK_p     ), spawn "passmenu")]
    systemctlKBs =
      map (\(k,v) -> (k, spawn $ "systemctl " ++ v))
        [ ((ms_, xK_F10), "suspend")
        , ((msc, xK_F11), "reboot")
        , ((msc, xK_F12), "poweroff")]
    miscKBs =
      [ ((const 0,   0x1008ffa9), spawn "synclient TouchpadOff=$(synclient -l | grep -c 'TouchpadOff.*=.*0')")
      , ((m__, xK_s      ), spawn "find-cursor")
      , ((msc, xK_s      ), spawn "xdotool mousemove 0 0; synclient TouchpadOff=$(synclient -l | grep -c 'TouchpadOff.*=.*0')")
      , ((m__, xK_z      ), spawn "myautosetup.pl --onlyIfChanged")
      , ((ms_, xK_z      ), spawn "myautosetup.pl")
      , ((msc, xK_z      ), spawn "myautosetup.pl --rotate=left --primOutNr=1")
#if 1
      , ((ms_, xK_y      ), spawn "xset s activate") -- screenlocker
#else
      , ((ms_, xK_y      ), spawn "slimlock") -- screenlocker
#endif

#if 1
      -- invert Colors (does not work with retdshift)
      , ((m__,  xK_Home   ), spawn "xrandr-invert-colors")
#else
      , ((m__,  xK_Home   ), spawn "xcalib -i -a")
#endif

      , ((m__,  xK_Print  ), spawn "screenshot.sh")
         -- or:
         -- - "bash -c \"import -frame ~/screen_`date +%Y-%m-%d_%H-%M-%S`.png\"")
         -- - "mkdir -p ~/_screenshots/ && scrot ~/_screenshots/screen_%Y-%m-%d_%H-%M-%S.png -d 1"

      -- keyboard layouts
      , ((m__,  xK_F2     ), spawn "feh ~/.xmonad/neo/neo_Ebenen_1_2_3_4.png")
      , ((m__,  xK_F3     ), spawn "feh ~/.xmonad/neo/neo_Ebenen_1_2_5_6.png")]

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
  spawn "pkill unclutter; unclutter"
  spawn "xset s 900"

------------------------------------------------------------------------
-- Log hook:
myLogHook xmproc = let
  myXmobarPP = def { ppOutput  = hPutStrLn xmproc . shortenStatus
                   , ppCurrent = xmobarColor maincolor "" . wrap "<" ">"
                   , ppSort    = scratchpadPPSort
                   , ppTitle   = (" " ++) . xmobarColor maincolor ""
                   , ppVisible = xmobarColor maincolor ""
                   }
  in dynamicLogWithPP myXmobarPP

------------------------------------------------------------------------
-- General

normalcolor = "#333333" :: String
maincolor = "#ee9a00" :: String
myConfig xmproc =
  applyMyBacklightControlLayer $
  applyMyVolumeControlLayer $
  applyMyUrgencyHook $
  applyMyScratchpads $
  applyMyFollowFocus $
  applyMyLayoutModifications $
  def { terminal           = "urxvtc"
      , borderWidth        = 3
      , modMask            = mod1Mask --  mod4Mask
      , normalBorderColor  = normalcolor
      , focusedBorderColor = maincolor
      , keys               = myKeys
      , layoutHook         = myLayout
      , manageHook         = myManageHook
      , startupHook        = myStartupHook
      , logHook            = myLogHook xmproc
      }

------------------------------------------------------------------------
-- Now run xmonad
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ myConfig xmproc
