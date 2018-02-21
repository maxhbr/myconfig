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
--
-- written by maximilian-huber.de
--
-- Last modified: Sa Jul 23, 2016  02:15
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
import           XMonad.Hooks.ManageDocks ( avoidStrutsOn
                                          , manageDocks
                                          , docksEventHook
                                          , ToggleStruts(..) )
import           XMonad.Hooks.ManageHelpers ( doCenterFloat )
import           XMonad.Hooks.UrgencyHook ( withUrgencyHook
                                          , focusUrgent )
import           XMonad.Hooks.SetWMName ( setWMName )

--------------------------------------------------------------------------------
-- Util
import           XMonad.Util.Run ( runProcessWithInput, spawnPipe )
import           XMonad.Util.Types ( Direction2D(..) )

--------------------------------------------------------------------------------
-- Actions
import           XMonad.Actions.CycleWS ( nextWS, prevWS
                                        , toggleWS'
                                        , shiftToNext, shiftToPrev
                                        , nextScreen, prevScreen
                                        , shiftNextScreen, shiftPrevScreen
                                        , moveTo
                                        , Direction1D(..)
                                        , WSType( NonEmptyWS ) )
import           XMonad.Actions.UpdatePointer ( updatePointer )
import           XMonad.Actions.GridSelect
import           XMonad.Actions.WindowGo ( runOrRaiseNext, raiseNext )

--------------------------------------------------------------------------------
-- Layouts
import           XMonad.Layout.BoringWindows( boringAuto
                                            , focusDown )
import           XMonad.Layout.Gaps (gaps)
import           XMonad.Layout.Named ( named )
import           XMonad.Layout.NoBorders ( smartBorders )
import           XMonad.Layout.Minimize ( minimize, minimizeWindow
                                        , MinimizeMsg(RestoreNextMinimizedWin) )
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.PerScreen (ifWider)
import           XMonad.Layout.PerWorkspace ( modWorkspaces )
import           XMonad.Layout.ResizableTile ( ResizableTall(ResizableTall)
                                             , MirrorResize ( MirrorShrink
                                                            , MirrorExpand ) )
import           XMonad.Layout.Spacing (spacing)
import           XMonad.Layout.TwoPane (TwoPane(TwoPane))
import           XMonad.Layout.IM -- (withIM)

import           XMonad.Layout.IfMax

--------------------------------------------------------------------------------
-- misc
import qualified Data.Map                    as M
import qualified XMonad.StackSet             as W

--------------------------------------------------------------------------------
-- MyConfig
import XMonad.MyConfig.Utils
import XMonad.MyConfig.Common
import XMonad.MyConfig.Scratchpads
import XMonad.MyConfig.ToggleFollowFocus
import XMonad.MyConfig.Notify

------------------------------------------------------------------------
-- Key bindings:
myKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
  map (\((m,k),v) -> ((m modm,k),v)) $
       basicKBs
    ++ miscKBs
    ++ scratchpadKBs
    ++ baclightControlKBs
    ++ layoutKBs
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

      , ((m__, xK_Tab   ), windows W.focusDown)
      , ((ms_, xK_Tab   ), focusDown)
      , ((m__, xK_u     ), focusUrgent)

      , ((m__, xK_j     ), windows W.focusDown)
      , ((m__, xK_k     ), windows W.focusUp)
      , ((ms_, xK_j     ), windows W.swapDown)
      , ((ms_, xK_k     ), windows W.swapUp)
      , ((m__, xK_o     ), spawn "urxvtc -e bash -c 'EDITOR=vim ranger'")
      , ((m_c, xK_Return), spawn "urxvtc -e zsh -c 'ssh vserver'")
      , ((ms_, xK_p     ), spawn "passmenu")]
      ++ switchWorkspaceKBs
      where
        switchWorkspaceKBs =
          -- mod-[1..9], Switch to workspace N
          -- mod-shift-[1..9], Move client to workspace N
          [((m, k), f i)
              | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
              , (f, m) <- [ (\i -> windows (W.greedyView i) >> popupCurDesktop, m__)
                          , (windows . W.shift, ms_) ]]
        {-
        switchPhysicalKBs =
          -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
          -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
          [((m .|. m__, k), screenWorkspace sc >>= flip whenJust (windows . f))
              | (k, sc) <- zip [xK_w, xK_e, xK_r] [0..]
              , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
         -}
    layoutKBs =
      [ ((m__, xK_space ), sendMessage NextLayout)
      , ((ms_, xK_x     ), sendMessage $ Toggle MIRROR)
      , ((m__, xK_f     ), sendMessage $ Toggle FULL)
      , ((ms_, xK_space ), setLayout $ XMonad.layoutHook conf)
      , ((m__, xK_m     ), withFocused minimizeWindow)
      , ((ms_, xK_m     ), sendMessage RestoreNextMinimizedWin)

      , ((m__, xK_t     ), withFocused $ windows . W.sink) -- Push window back into tiling
      , ((m__, xK_comma ), sendMessage (IncMasterN 1))
      , ((m__, xK_period), sendMessage (IncMasterN (-1)))

      -- Shrink and Expand
      , ((m__, xK_h     ), sendMessage Shrink)
      , ((m__, xK_l     ), sendMessage Expand)
      , ((ms_, xK_h     ), sendMessage MirrorShrink)
      , ((ms_, xK_l     ), sendMessage MirrorExpand)

      , ((m__, xK_b     ), sendMessage ToggleStruts)]
      ++ cycleWSKBs
      ++ map (\(a,b) -> (a,b >> popupCurDesktop))
           [ ((m__, xK_i), runOrRaiseNext "firefox" (className =? "Firefox" <||> className =?  "chromium-browser" <||> className =? "Chromium-browser"))
           , ((m__, 0xfc), runOrRaiseNext "~/bin/ec" (className =? "Emacs"))
           , ((m__, 0xf6), raiseNext (className =? "jetbrains-phpstorm" <||> className =? "jetbrains-idea"))
           ]
      -- ++ combineTwoKBs
      -- ++ subLayoutKBs
      where
        cycleWSKBs = map (\(a,b) -> (a,b >> popupCurDesktop))
          [ ((m__, xK_Down ), moveTo Next NonEmptyWS) -- HiddenNonEmptyWS
          , ((m__, xK_Up   ), moveTo Prev NonEmptyWS) -- HiddenNonEmptyWS
          , ((ms_, xK_Down ), shiftToNext >> nextWS)
          , ((ms_, xK_Up   ), shiftToPrev >> prevWS)
#if 1
          , ((m__, xK_Left ), nextScreen)
          , ((m__, xK_Right), prevScreen)
          , ((ms_, xK_Left ), shiftNextScreen)
          , ((ms_, xK_Right), shiftPrevScreen)
#else
          , ((m__, xK_Right), nextScreen)
          , ((m__, xK_Left ), prevScreen)
          , ((ms_, xK_Right), shiftNextScreen)
          , ((ms_, xK_Left ), shiftPrevScreen)
#endif
          , ((m__, xK_y    ), toggleWS' ["NSP"])
          , ((m__, xK_a    ), toggleWS' ["NSP"])]
        -- combineTwoKBs =
        --   [((msc, xK_l ), sendMessage $ Move L)]
        -- subLayoutKBs =
        --   map (\(k,v) -> ((m_c, k), sendMessage $ pullGroup v))
        --     [(xK_h,L),(xK_l,R),(xK_k,U),(xK_j,D)]
        --     ++ [ ((m_c, xK_m), withFocused (sendMessage . MergeAll))
        --        , ((m_c, xK_u), withFocused (sendMessage . UnMerge))]
    systemctlKBs =
      map (\(k,v) -> (k, spawn $ "systemctl " ++ v))
        [ ((ms_, xK_F10), "suspend")
        , ((msc, xK_F11), "reboot")
        , ((msc, xK_F12), "poweroff")]
    miscKBs =
      [ ((const 0,   0x1008ffa9), spawn "synclient TouchpadOff=$(synclient -l | grep -c 'TouchpadOff.*=.*0')")
      , ((m__, xK_s      ), spawn "find-cursor")
      , ((ms_, xK_s      ), spawn "xdotool mousemove 0 0; synclient TouchpadOff=$(synclient -l | grep -c 'TouchpadOff.*=.*0')")
      , ((m__, xK_z      ), spawn "~/bin/myautosetup.pl --onlyIfChanged")
      , ((ms_, xK_z      ), spawn "~/bin/myautosetup.pl")
      , ((msc, xK_z      ), spawn "~/bin/myautosetup.pl --rotate=left --primOutNr=1")
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

      , ((m__,  xK_Print  ), spawn "~/bin/screenshot.sh") -- or: "bash -c \"import -frame ~/screen_`date +%Y-%m-%d_%H-%M-%S`.png\"")
      , ((ms_,  xK_Print  ), spawn "scrot ~/screen_%Y-%m-%d_%H-%M-%S.png -d 1") -- screenshot

      -- keyboard layouts
      , ((m__,  xK_F2     ), spawn "feh ~/.xmonad/neo/neo_Ebenen_1_2_3_4.png")
      , ((m__,  xK_F3     ), spawn "feh ~/.xmonad/neo/neo_Ebenen_1_2_5_6.png")]
      ++ volumeControlls
      where
        volumeControlls =
#if 1
-- pulseaudio
          map (\(k,args) -> ((const 0, k)
                         , runProcessWithInput "/home/mhuber/.xmonad/bin/mypamixer.sh" args ""
                           >>= myDefaultPopup . ("V: " ++)
                         ))
            [ (0x1008ff12, ["mute"])
            , (0x1008ff11, ["-10%"])
            , (0x1008ff13, ["+10%"])]
#else
-- alsa
          map (\(k,v) -> ((const 0, k)
                         , runProcessWithInput "/home/mhuber/.xmonad/bin/myamixer.sh" v ""
                           >>= myDefaultPopup
                         ))
            [ (0x1008ff12, ["toggle"])
            , (0x1008ff11, ["6dB-"])
            , (0x1008ff13, ["unmute","3dB+"])]
#endif
    baclightControlKBs =
      [((m__, xK_F1), spawnSelected def [ "xbacklight =50"
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

------------------------------------------------------------------------
-- Mouse bindings:
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
  [ ((modm, button1), \w -> focus w
                            >> mouseMoveWindow w
                            >> windows W.shiftMaster)
  , ((modm, button2), \w -> focus w
                            >> windows W.shiftMaster)
  , ((modm, button3), \w -> focus w
                            >> mouseResizeWindow w
                            >> windows W.shiftMaster) ]

------------------------------------------------------------------------
-- Layouts / workspaces:
myWorkspaces = map show [1..7] ++ ("web" : map show [9..10]) ++ ["vbox", "media"]

myLayout = smartBorders $
           boringAuto $
           modWorkspaces [ "vbox", "media" ] (Full |||) $
           avoidStrutsOn[U,D] $
           named "" $
           withIM (1%7) (Title "Tabs Outliner") $
           mkToggle (single FULL) $
           mkToggle (single MIRROR) $
           IfMax 1 full  (IfMax 2 tiled (tiled ||| dtb) ||| full)
  where
    baseSpacing = 10
    wqhdSpacing = 20
    wqhdGapping = (2560 - 1920) `div` 2 - wqhdSpacing + baseSpacing
    myUprightGapping l = ifWider 1440 l $
                         ifWider 1439 (gaps [(U,wqhdGapping), (D,wqhdGapping)] l) l
    myUprightMirroring l = ifWider 1440 l $
                           ifWider 1439 (Mirror l) l
    mySpacing l = ifWider 1920 (spacing wqhdSpacing l) $
                  ifWider 1919 (spacing baseSpacing l) $
                  ifWider 1439 (spacing wqhdSpacing l)
                  l
    full      = named "=" $
                mySpacing $
                ifWider 1920 (gaps [(L,wqhdGapping), (R,wqhdGapping)] Full) $
                myUprightGapping Full
    tiled     = named " " $
                minimize $
                mySpacing $
                myUprightGapping $
                myUprightMirroring $
                ResizableTall 1 (3/100) (1/2) []
    dtb       = named "%" $
                minimize $
                mySpacing $
                myUprightGapping $
                myUprightMirroring $
                TwoPane (3/100) (1/2)

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
myManageHook = (composeAll (foldMap (\(a,cs) -> map (\c -> className =? c --> a) cs)
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
                                          ,"kdesktop"]) ]))
               <+> scratchpadHook
               <+> manageDocks

------------------------------------------------------------------------
-- Event handling:
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = fullscreenEventHook
              <+> docksEventHook
              <+> focusFollow

------------------------------------------------------------------------
-- Startup hook:
myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"
  spawn "pkill unclutter; unclutter"
  spawn "xset s 900"
  spawn "xss-lock -- slimlock"

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
     >> updatePointer (0.5,0.5) (0.5,0.5)

------------------------------------------------------------------------
-- General

maincolor = "#ee9a00" :: String
myConfig xmproc = withUrgencyHook myUrgencyHook $
  def { terminal           = "urxvtc"
      , focusFollowsMouse  = False -- see: focusFollow
      , borderWidth        = 3
      -- , modMask            = mod4Mask
      , modMask            = mod1Mask
      , workspaces         = myWorkspaces
      , normalBorderColor  = "#333333"
      , focusedBorderColor = maincolor -- "#dd0000"
      , keys               = myKeys
      , mouseBindings      = myMouseBindings
      , layoutHook         = myLayout
      , manageHook         = myManageHook
      , handleEventHook    = myEventHook
      , startupHook        = myStartupHook
      , logHook            = myLogHook xmproc
      }

------------------------------------------------------------------------
-- Now run xmonad
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ myConfig xmproc
