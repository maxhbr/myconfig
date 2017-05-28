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

------------------------------------------------------------------------
-- Imports:
import           Data.Monoid
import           Data.Foldable (foldMap)
import           Control.Monad
import           System.Exit ( exitSuccess )
import           System.IO ( hPutStrLn )
import           XMonad
import           Graphics.X11.ExtraTypes.XF86()

import           XMonad.Prompt ( defaultXPConfig )
import           XMonad.Prompt.Shell ( shellPrompt )

import           XMonad.Hooks.DynamicLog ( dynamicLogWithPP
                                         , xmobarPP , defaultPP
                                         , PP(..)
                                         , xmobarColor
                                         , wrap )
import           XMonad.Hooks.EwmhDesktops ( fullscreenEventHook )
import           XMonad.Hooks.ManageDocks ( avoidStrutsOn
                                          , manageDocks
                                          , docksEventHook
                                          , ToggleStruts(..) )
import           XMonad.Hooks.ManageHelpers ( doFullFloat
                                            , doCenterFloat )
import           XMonad.Hooks.UrgencyHook ( withUrgencyHook
                                          , NoUrgencyHook(..) )
import           XMonad.Hooks.SetWMName ( setWMName )

import           XMonad.Util.Run ( spawnPipe )
import           XMonad.Util.Types ( Direction2D(..) )

import           XMonad.Actions.CycleWS ( nextWS, prevWS
                                        , toggleWS'
                                        , shiftToNext, shiftToPrev
                                        , nextScreen, prevScreen
                                        , shiftNextScreen, shiftPrevScreen
                                        , moveTo
                                        , Direction1D(..)
                                        , WSType( NonEmptyWS )
                                        , skipTags )
import           XMonad.Actions.UpdatePointer ( updatePointer )
import           XMonad.Actions.GridSelect

import           XMonad.Layout.BoringWindows( boringAuto
                                            , focusDown )
import           XMonad.Layout.Named ( named )
import           XMonad.Layout.NoBorders ( smartBorders )
import           XMonad.Layout.Minimize ( minimize, minimizeWindow
                                        , MinimizeMsg(RestoreNextMinimizedWin) )
import           XMonad.Layout.PerWorkspace ( onWorkspace, onWorkspaces, modWorkspace, modWorkspaces )
import           XMonad.Layout.ResizableTile ( ResizableTall(ResizableTall)
                                             , MirrorResize ( MirrorShrink
                                                            , MirrorExpand ) )
import           XMonad.Layout.Simplest ( Simplest(Simplest) )
-- import           XMonad.Layout.SubLayouts ( subLayout
--                                           , pullGroup
--                                           , GroupMsg( MergeAll, UnMerge ) )
import           XMonad.Layout.Tabbed ( addTabs
                                      , shrinkText
                                      , tabbedBottom
                                      , Theme(..) )
import           XMonad.Layout.Magnifier (magnifiercz)
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Master (mastered)
-- testing layouts:
import           XMonad.Layout.TwoPane (TwoPane(TwoPane))
import           XMonad.Layout.IfMax (IfMax(IfMax), ifMax)
import           XMonad.Layout.Spacing
import           XMonad.Layout.Gaps -- testing
import           XMonad.Layout.PerScreen -- testing
import           XMonad.Layout.Grid -- testing
-- import           XMonad.Layout.WorkspaceDir

import qualified Data.Map                    as M
import qualified XMonad.StackSet             as W

import XMonad.MyConfig.Common
import XMonad.MyConfig.Scratchpads
import XMonad.MyConfig.ToggleFollowFocus

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
    -- ++ mpdKBs
  where
    basicKBs =
      [ ((ms_            , xK_Return), spawn $ XMonad.terminal conf)
      , ((msc, xK_Return), spawn "urxvtd -q -f -o &")
      -- , ((m__, xK_Return), windows W.swapMaster)
      , ((m4m, xK_Return), windows W.swapMaster)
      , ((m__, xK_g     ), spawn "~/bin/emc || emacs")
      , ((m__, xK_q     ), spawn "xmonad --recompile; xmonad --restart") -- Restart xmonad
      , ((msc, xK_q     ), io exitSuccess) -- Quit xmonad
      -- , ((m__, xK_p     ), spawn "`dmenu_run`")
      , ((m__, xK_p     ), spawn "`dmenu_path | yeganesh`")
      , ((m__, xK_x     ), shellPrompt defaultXPConfig)
      -- , ((ms_, xK_x     ), changeDir def)


      , ((ms_, xK_c     ), kill) -- close focused window

      , ((m__, xK_Tab   ), windows W.focusDown) -- Move focus to the next window
      , ((ms_, xK_Tab   ), focusDown) -- Move focus to the next window using BoringWindows

      , ((m__, xK_j     ), windows W.focusDown) -- Move focus to the next window
      , ((m__, xK_k     ), windows W.focusUp  ) -- Move focus to the previous window
      , ((ms_, xK_j     ), windows W.swapDown  ) -- Swap the focused window with the next window
      , ((ms_, xK_k     ), windows W.swapUp    ) -- Swap the focused window with the previous window
      , ((m__, xK_o     ), spawn "urxvtc -e bash -c 'EDITOR=vim ranger'")
      , ((m_c, xK_Return), spawn "urxvtc -e zsh -c 'ssh vserver'")
      , ((ms_, xK_p     ), spawn "passmenu")]
      ++ switchWorkspaceKBs
      where
        switchWorkspaceKBs =
          -- mod-[1..9,0], Switch to workspace N
          -- mod-shift-[1..9,0], Move client to workspace N
          [((m, k), windows $ f i)
              | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
              , (f, m) <- [(W.greedyView, m__), (W.shift, ms_)]]
        {-
        switchPhysicalKBs =
          -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
          -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
          [((m .|. m__, k), screenWorkspace sc >>= flip whenJust (windows . f))
              | (k, sc) <- zip [xK_w, xK_e, xK_r] [0..]
              , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
         -}
    layoutKBs =
      [ ((m__, xK_space ), sendMessage NextLayout) -- Rotate through the available layout algorithms
      , ((ms_, xK_x     ), sendMessage $ Toggle MIRROR)
      , ((m__, xK_f     ), sendMessage $ Toggle FULL)
      , ((ms_, xK_space ), setLayout $ XMonad.layoutHook conf) --  Reset the layouts on the current workspace to default
      , ((m__, xK_m     ), withFocused minimizeWindow)
      , ((ms_, xK_m     ), sendMessage RestoreNextMinimizedWin)

      , ((m__, xK_t     ), withFocused $ windows . W.sink) -- Push window back into tiling
      , ((m__, xK_comma ), sendMessage (IncMasterN 1)) -- Increment the number of windows in the master area
      , ((m__, xK_period), sendMessage (IncMasterN (-1))) -- Deincrement the number of windows in the master area

      -- Shrink and Expand
      , ((m__, xK_h     ), sendMessage Shrink)
      , ((m__, xK_l     ), sendMessage Expand)
      , ((ms_, xK_h     ), sendMessage MirrorShrink)
      , ((ms_, xK_l     ), sendMessage MirrorExpand)

      , ((m__, xK_b     ), sendMessage ToggleStruts)]
      ++ cycleWSKBs
      -- ++ combineTwoKBs
      -- ++ subLayoutKBs
      where
        cycleWSKBs =
          [ ((m__, xK_Down ), moveTo Next NonEmptyWS) -- HiddenNonEmptyWS
          , ((m__, xK_Up   ), moveTo Prev NonEmptyWS) -- HiddenNonEmptyWS
          , ((ms_, xK_Down ), shiftToNext >> nextWS)
          , ((ms_, xK_Up   ), shiftToPrev >> prevWS)
          , ((m__, xK_Right), nextScreen)
          , ((m__, xK_Left ), prevScreen)
          , ((ms_, xK_Right), shiftNextScreen)
          , ((ms_, xK_Left ), shiftPrevScreen)
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
      , ((ms_, xK_s      ), spawn "xdotool mousemove 0 0; synclient TouchpadOff=$(synclient -l | grep -c 'TouchpadOff.*=.*0')")
      , ((ms_, xK_z      ), spawn "~/bin/myautosetup.pl --primOutNr=1") -- auto
      , ((msc, xK_z      ), spawn "~/bin/myautosetup.pl --rotate=left --primOutNr=1") -- auto
      -- , ((ms_, xK_y      ), spawn "slimlock") -- screenlocker
      , ((ms_, xK_y      ), spawn "xset s activate") -- screenlocker

      , ((m__,  xK_Home   ), spawn "xcalib -i -a") --invert Colors (does not work with retdshift)

      , ((m__,  xK_Print  ), spawn "scrot ~/screen_%Y-%m-%d_%H-%M-%S.png -d 1") -- screenshot
      , ((ms_,  xK_Print  ), spawn "bash -c \"import -frame ~/screen_`date +%Y-%m-%d_%H-%M-%S`.png\"")

      -- keyboard layouts
      , ((m__,  xK_F2     ), spawn "feh ~/.xmonad/neo/neo_Ebenen_1_2_3_4.png")
      , ((m__,  xK_F3     ), spawn "feh ~/.xmonad/neo/neo_Ebenen_1_2_5_6.png")]
      ++ volumeControlls
      where
        volumeControlls =
          map (\(k,v) -> ((const 0, k), spawn v))
            [ (0x1008ff12, "amixer -q set Master toggle")
            , (0x1008ff11, "amixer -q set Master 6dB-")
            , (0x1008ff13, "amixer -q set Master unmute 3dB+")]
    -- mpdKBs =
    --   [ ((m__, 0xfc), spawn "mpc -h mpd@192.168.178.26 --no-status prev")
    --   , ((ms_, 0xfc), spawn "mpc -h mpd@192.168.178.26 --no-status volume -10")
    --   , ((m__, 0xf6), spawn "mpc -h mpd@192.168.178.26 --no-status toggle")
    --   , ((m__, 0xe4), spawn "mpc -h mpd@192.168.178.26 --no-status next")
    --   , ((ms_, 0xe4), spawn "mpc -h mpd@192.168.178.26 --no-status volume +5")]
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
myWorkspaces = map show [1..7] ++ ("web" : map show [9..15]) ++ ["vbox", "media"]

myLayout = mkToggle (single FULL) $
           smartBorders $
           boringAuto $
           modWorkspaces [ "vbox", "media" ] (Full |||) $
           avoidStrutsOn[U,D] $
           mkToggle (single MIRROR) $
           full ||| dtb ||| tiled
           -- (IfMax 1 full  (full ||| (IfMax 2 tiled (dtb ||| tiled))))
  where
    -- full1080 = named "fixed=" $
    --            ifWider 1920 (gaps [(L,320),(U,180),(R,320),(D,180)] Full) Full
    baseSpacing = 10
    wqhdSpacing = 20
    wqhdGapping = (2560 - 1920) `div` 2 - wqhdSpacing + baseSpacing
    mySpacing l = ifWider 1920 (spacing wqhdSpacing l)
                               (ifWider 1919 (spacing baseSpacing l) l)
    full      = named "=" $
                mySpacing $
                ifWider 1920 (gaps [(L,wqhdGapping), (R,wqhdGapping)] Full) Full
    tiled     = named " " $
                minimize $
                -- addTabs shrinkText myTab $
                mySpacing $
                -- subLayout [] Simplest $
                ResizableTall 1 (3/100) (1/2) []
    -- dtb     = named "%" $
    --     mySpacing $
    --     mastered (1/100) (1/2) $
    --     gaps [(D,10)] $
    --     tabbedBottom shrinkText myTab
    dtb       = named "%" $
                minimize $
                mySpacing $
                TwoPane (3/100) (1/2)
    --options:
    -- myTab     = def { activeColor         = "black"
    --                 , inactiveColor       = "black"
    --                 , urgentColor         = "yellow"
    --                 , activeBorderColor   = "orange"
    --                 , inactiveBorderColor = "#333333"
    --                 , urgentBorderColor   = "black"
    --                 , activeTextColor     = "orange"
    --                 , inactiveTextColor   = "#666666"
    --                 , decoHeight          = 14 }

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
                                               ,"qemu"
                                               ,"qemu-system-x86_64"
                                               ,"feh"
                                               ,"Zenity"])
                             , (doFloat, ["MPlayer"
                                         ,"Onboard"])
                             , (doShift "web", ["Firefox"]) --["Chromium" ,"chromium-browser" ,"Firefox"]
                             , (doShift "vbox", ["Virtualbox"
                                                ,"VirtualBox"])
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
  spawn "killall unclutter; unclutter"
  spawn "xset s 900"
  spawn "xss-lock -- slimlock"

------------------------------------------------------------------------
-- General

myConfig xmproc = withUrgencyHook NoUrgencyHook $
  def { terminal             = "urxvtc"
      , focusFollowsMouse  = False -- see: focusFollow
      , borderWidth        = 2
      -- , modMask            = mod4Mask
      , modMask            = mod1Mask
      , workspaces         = myWorkspaces
      , normalBorderColor  = "#333333"
      , focusedBorderColor = "#dd0000"
      , keys               = myKeys
      , mouseBindings      = myMouseBindings
      , layoutHook         = myLayout
      , manageHook         = myManageHook
      , handleEventHook    = myEventHook
      , startupHook        = myStartupHook
      , logHook            = dynamicLogWithPP xmobarPP
                             { ppOutput  = hPutStrLn xmproc
                             , ppCurrent = xmobarColor "#ee9a00" "" . wrap "<" ">"
                             , ppSort    = scratchpadPPSort
                             , ppTitle   = (" " ++) . xmobarColor "#ee9a00" ""
                             , ppVisible = xmobarColor "#ee9a00" ""
                             } >> updatePointer (0.5,0.5) (0.1,0.1)
      }

------------------------------------------------------------------------
-- Now run xmonad
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ myConfig xmproc
