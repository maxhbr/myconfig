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
--  slock        screenlock tool
--  unclutter    to hide mouse pointer
--  urxvt        terminal
--  xcalib       invert colors
--  xmobar       bar
--  pass         password manager
--
-- written by maximilian-huber.de
--
-- Last modified: Sa Sep 12, 2015  05:12
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -W -fwarn-unused-imports -fno-warn-missing-signatures #-}

------------------------------------------------------------------------
-- Imports:
import           Data.Monoid
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
                                          , ToggleStruts(..) )
import           XMonad.Hooks.ManageHelpers ( doFullFloat
                                            , doCenterFloat )
import           XMonad.Hooks.UrgencyHook ( withUrgencyHook
                                          , NoUrgencyHook(..) )
import           XMonad.Hooks.SetWMName ( setWMName )

import           XMonad.Util.NamedScratchpad ( NamedScratchpad(..)
                                             , customFloating , nonFloating
                                             , namedScratchpadAction
                                             , namedScratchpadFilterOutWorkspace
                                             , namedScratchpadManageHook )
import           XMonad.Util.Run ( spawnPipe )
import           XMonad.Util.Types ( Direction2D(..) )

import           XMonad.Actions.CycleWS ( nextWS, prevWS
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
import           XMonad.Layout.LayoutCombinators  ( (*||*) )
import           XMonad.Layout.Named ( named )
import           XMonad.Layout.NoBorders ( smartBorders )
import           XMonad.Layout.Minimize ( minimize, minimizeWindow
                                        , MinimizeMsg(RestoreNextMinimizedWin) )
import           XMonad.Layout.PerWorkspace ( onWorkspace )
import           XMonad.Layout.ResizableTile ( ResizableTall(ResizableTall)
                                             , MirrorResize ( MirrorShrink
                                                            , MirrorExpand ) )
import           XMonad.Layout.Simplest ( Simplest(Simplest) )
import           XMonad.Layout.SubLayouts ( subLayout
                                          , pullGroup
                                          , GroupMsg( MergeAll, UnMerge ) )
import           XMonad.Layout.Tabbed ( addTabs
                                      , shrinkText
                                      , tabbedBottom
                                      , defaultTheme
                                      , Theme(..) )
import           XMonad.Layout.WindowNavigation ( configurableNavigation
                                                , navigateColor
                                                , Navigate(Move))
import           XMonad.Layout.Magnifier (magnifiercz)
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances

import qualified Data.Map                    as M
import qualified XMonad.StackSet             as W
import qualified XMonad.Util.ExtensibleState as XS

------------------------------------------------------------------------
-- Key bindings:
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
       basicKBs
    ++ miscKBs
    ++ scratchpadKBs
    ++ baclightControlKBs 
    ++ layoutKBs
    ++ systemctlKBs
    ++ mpdKBs
  where
    basicKBs =
        [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
        , ((modm .|. shiftMask .|. controlMask, xK_Return), spawn "urxvtd -q -f -o &")
        , ((modm,                xK_g    ), spawn "~/bin/emc || emacs")
        , ((modm,                xK_q    ), spawn "xmonad --recompile; xmonad --restart") -- Restart xmonad
        , ((modm .|. shiftMask .|. controlMask, xK_q     ), io exitSuccess) -- Quit xmonad
        , ((modm,               xK_p     ), spawn "`dmenu_path | yeganesh`")
        , ((modm,               xK_x     ), shellPrompt defaultXPConfig)


        , ((modm .|. shiftMask, xK_c     ), kill) -- close focused window

        , ((modm,               xK_Tab   ), windows W.focusDown) -- Move focus to the next window
        , ((modm .|. shiftMask, xK_Tab   ), focusDown) -- Move focus to the next window using BoringWindows

        , ((modm,               xK_j     ), windows W.focusDown) -- Move focus to the next window
        , ((modm,               xK_k     ), windows W.focusUp  ) -- Move focus to the previous window
        , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  ) -- Swap the focused window with the next window
        , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    ) -- Swap the focused window with the previous window
        , ((modm,               xK_o     ), spawn "urxvtc -e bash -c 'EDITOR=vim ranger'")
        , ((modm .|. controlMask, xK_Return), spawn "urxvtc -e zsh -c 'ssh vserver'")
        , ((modm .|. shiftMask, xK_p     ), spawn "passmenu")]
          ++ switchWorkspaceKBs
      where
        switchWorkspaceKBs =
          -- mod-[1..9], Switch to workspace N
          -- mod-shift-[1..9], Move client to workspace N
          [((m .|. modm,           k        ), windows $ f i)
              | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
              , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
        {-
        switchPhysicalKBs =
          -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
          -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
          [((m .|. modm,           key      ), screenWorkspace sc >>= flip whenJust (windows . f))
              | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
              , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
         -}
    layoutKBs = 
        [ ((modm .|. shiftMask, xK_x     ), sendMessage NextLayout) -- Rotate through the available layout algorithms
        , ((modm,               xK_space ), sendMessage $ Toggle FULL)
        -- , ((), sendMessage $ Toggle FULL)
        , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) --  Reset the layouts on the current workspace to default
        , ((modm,               xK_m     ), withFocused minimizeWindow)
        , ((modm .|. shiftMask, xK_m     ), sendMessage RestoreNextMinimizedWin)

        , ((modm,               xK_t     ), withFocused $ windows . W.sink) -- Push window back into tiling
        , ((modm,               xK_comma ), sendMessage (IncMasterN 1)) -- Increment the number of windows in the master area
        , ((modm,               xK_period), sendMessage (IncMasterN (-1))) -- Deincrement the number of windows in the master area

        -- Shrink and Expand
        , ((modm,               xK_h     ), sendMessage Shrink)
        , ((modm,               xK_l     ), sendMessage Expand)
        , ((modm .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
        , ((modm .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

        , ((modm,               xK_b     ), sendMessage ToggleStruts)]
        ++ cycleWSKBs
        ++ combineTwoKBs
        ++ subLayoutKBs
      where
        cycleWSKBs = 
          [ ((modm,                xK_Down  ), moveTo Next NonEmptyWS)
          , ((modm,                xK_Up    ), moveTo Prev NonEmptyWS)
          , ((modm .|. shiftMask,  xK_Down  ), shiftToNext >> nextWS)
          , ((modm .|. shiftMask,  xK_Up    ), shiftToPrev >> prevWS)
          , ((modm,                xK_Right ), nextScreen)
          , ((modm,                xK_Left  ), prevScreen)
          , ((modm .|. shiftMask,  xK_Right ), shiftNextScreen)
          , ((modm .|. shiftMask,  xK_Left  ), shiftPrevScreen)
          , ((modm,                xK_y     ), toggleSkip ["NSP"])
          , ((modm,                xK_a     ), toggleSkip ["NSP"])]
        combineTwoKBs = 
          [((modm .|. controlMask .|. shiftMask, xK_l ), sendMessage $ Move L)]
        subLayoutKBs = 
          [ ((modm .|. controlMask, xK_h), sendMessage $ pullGroup L)
          , ((modm .|. controlMask, xK_l), sendMessage $ pullGroup R)
          , ((modm .|. controlMask, xK_k), sendMessage $ pullGroup U)
          , ((modm .|. controlMask, xK_j), sendMessage $ pullGroup D)

          , ((modm .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
          , ((modm .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))
          ]
    systemctlKBs = 
      [ ((modm .|. shiftMask,  xK_F10  ),  spawn "systemctl suspend")
      , ((modm .|. shiftMask .|. controlMask,  xK_F11  ),  spawn "systemctl reboot")
      , ((modm .|. shiftMask .|. controlMask,  xK_F12  ),  spawn "systemctl poweroff") ]
    miscKBs =
        [ ((0,                  0x1008ffa9), spawn "synclient TouchpadOff=$(synclient -l | grep -c 'TouchpadOff.*=.*0')")
        , ((modm .|. shiftMask, xK_s     ), spawn "xdotool mousemove 0 0; synclient TouchpadOff=$(synclient -l | grep -c 'TouchpadOff.*=.*0')")
        , ((modm .|. shiftMask, xK_z), spawn "~/bin/myautosetup.pl --primOutNr=1") -- auto
        , ((modm .|. shiftMask .|. controlMask, xK_z), spawn "~/bin/myautosetup.pl --rotate=left --primOutNr=1") -- auto
        , ((0,                0x1008ff59), spawn "~/bin/myautosetup.sh")
        , ((modm .|. shiftMask,  xK_y    ), spawn "slock") -- screenlocker

        , ((modm,                xK_Home ), spawn "xcalib -i -a") --invert Colors

        -- screenshot
        , ((modm, xK_Print), spawn "scrot ~/screen_%Y-%m-%d_%H-%M-%S.png -d 1")
        , ((modm .|. shiftMask, xK_Print), spawn "bash -c \"import -frame ~/screen_`date +%Y-%m-%d_%H-%M-%S`.png\"")

        , ((modm,                xK_s     ), toggleFF) -- toggle mouse follow focus

        -- keyboard layouts
        , ((modm, xK_F2), spawn "feh ~/.xmonad/neo/neo_Ebenen_1_2_3_4.png")
        , ((modm, xK_F3), spawn "feh ~/.xmonad/neo/neo_Ebenen_1_2_5_6.png")]
        ++ volumeControlls
      where
        volumeControlls = 
          [ ((0,                  0x1008ff12), spawn "amixer -q set Master toggle")
          , ((0,                  0x1008ff11), spawn "amixer -q set Master 6dB-")
          , ((0,                  0x1008ff13), spawn "amixer -q set Master unmute 3dB+")]
    mpdKBs = 
      [ ((modm,   0xfc ), spawn "mpc -h mpd@192.168.178.61 --no-status prev")
      , ((modm .|. shiftMask, 0xfc ), spawn "mpc -h mpd@192.168.178.61 --no-status volume -10")
      , ((modm, 0xf6 ), spawn "mpc -h mpd@192.168.178.61 --no-status toggle")
      , ((modm .|. shiftMask, 0xf6 ), namedScratchpadAction scratchpads "ncmpcpp")
      , ((modm, 0xe4 ), spawn "mpc -h mpd@192.168.178.61 --no-status next")
      , ((modm .|. shiftMask, 0xe4 ), spawn "mpc -h mpd@192.168.178.61 --no-status volume +5")]
    baclightControlKBs =
      [ ((modm, xK_F1), spawnSelected defaultGSConfig [ "xbacklight =50"
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
                                                      , "xbacklight =0"])]
    scratchpadKBs =
      [ ((modm .|. shiftMask,  xK_minus ),
          namedScratchpadAction scratchpads "scratchpad")
      , ((modm,                xK_i     ),
          namedScratchpadAction scratchpads "ScratchWeb")
      , ((modm .|. shiftMask,  xK_i     ),
          namedScratchpadAction scratchpads "ScratchMutt")
      , ((modm,                xK_n     ),
          namedScratchpadAction scratchpads "notepad")]

-- Toggle workspaces but ignore some
toggleSkip :: [WorkspaceId] -> X ()
toggleSkip skips = do
    hs <- gets (flip skipTags skips . W.hidden . windowset)
    unless (null hs) (windows . W.view . W.tag $ head hs)

------------------------------------------------------------------------
-- Mouse bindings:
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:
myLayout = avoidStrutsOn[U,D] $
    smartBorders $
    configurableNavigation (navigateColor "#333333") $
    boringAuto $
    mkToggle (MIRROR ?? FULL ?? EOT) $
    onWorkspace "1" (tiled ||| full ||| dtb) $
    onWorkspace "5" (dtb ||| magnifiercz 1.3 dtb ||| full) $
    onWorkspace "6" (dtb ||| full) $
    onWorkspace "7" (dtb ||| full) $
    onWorkspace "web" (full ||| tiled)
    (tiled ||| full ||| dtb )
    where
        tiled   = named " " $
            minimize $
            addTabs shrinkText myTab $
            subLayout [] Simplest $
            ResizableTall nmaster delta ratio []
        full    = named "=" $
            minimize Full
        dtb     = named "%" $
            minimize $
            tabbedBottom shrinkText myTab *||* tiled
        --options:
        nmaster = 1
        ratio   = 1/2
        delta   = 3/100
        myTab   = defaultTheme
            { activeColor         = "black"
            , inactiveColor       = "black"
            , urgentColor         = "yellow"
            , activeBorderColor   = "orange"
            , inactiveBorderColor = "#333333"
            , urgentBorderColor   = "black"
            , activeTextColor     = "orange"
            , inactiveTextColor   = "#666666"
            , decoHeight          = 14 }
        
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
myManageHook = composeAll
    [ className =? "Xmessage"                      --> doCenterFloat
    , role      =? "gimp-message-dialog"           --> doCenterFloat
    , className =? "MPlayer"                       --> doFloat
    , className =? "Onboard"                       --> doFloat
    , className =? "com-mathworks-util-PostVMInit" --> doShift "7"
    , className =? "Chromium"                      --> doShift "web"
    , role      =? "app"                           --> doFloat
    , className =? "Virtualbox"                    --> doFullFloat
    , className =? "qemu"                          --> doCenterFloat
    , className =? "qemu-system-x86_64"            --> doCenterFloat
    , className =? "feh"                           --> doCenterFloat
    , className =? "Steam"                         --> doShift "9"
    , resource  =? "desktop_window"                --> doIgnore
    , resource  =? "kdesktop"                      --> doIgnore
    , className =? "Zenity"                        --> doCenterFloat ]
        <+> composeAll
            [resource  =? ("ToWorkspace"++i) --> doShift i | i <- myWorkspaces]
        <+> composeAll
            [className =? ("ToWorkspace"++i) --> doShift i | i <- myWorkspaces]
        <+> namedScratchpadManageHook scratchpads
        <+> manageDocks
  where role = stringProperty "WM_WINDOW_ROLE"

-- Scratchpads
--
scratchpads :: [NamedScratchpad]
scratchpads =
    [ NS "scratchpad" "urxvtc -name Scratchpad -e ~/.xmonad/tmux-scratch.sh"
        (resource =? "Scratchpad")
        (customFloating $ W.RationalRect (1/12) (1/10) (5/6) (4/5))
    , NS "ScratchWeb" "Chromium" (resource =? "Chromium") nonFloating
        {-(customFloating $ W.RationalRect (1/64) (3/128) (31/32) (31/32))-}
    , NS "ncmpcpp" "urxvtc -name Ncmpcpp -e ncmpcpp"
        (resource =? "Ncmpcpp")
        (customFloating $ W.RationalRect (1/2) (1/5) (1/2) (4/5))
    , NS "notepad" "/usr/bin/emacsclient -a \"\" -nc ~/Sync/org/index.org"
        (resource =? "Notepad")
        (customFloating $ W.RationalRect (4/12) (3/20) (7/12) (4/5))
   , NS "ScratchMutt" "urxvtc -name ScratchMutt -e bash -c \"~/bin/mailclient.sh\""
       (resource =? "ScratchMutt")
       (customFloating $ W.RationalRect (1/24) (3/20) (5/6) (4/5))
   , NS "ScratchAlsa" "urxvtc -name ScratchAlsa -e bash -c alsamixer"
       (resource =? "ScratchAlsa")
       (customFloating $ W.RationalRect (1/24) (3/20) (5/6) (4/5))
   , NS "ScratchHtop" "urxvtc -name ScratchHtop -e bash -c htop"
       (resource =? "ScratchHtop")
       (customFloating $ W.RationalRect (1/10) (1/10) (4/5) (4/5)) ]

------------------------------------------------------------------------
-- Event handling:
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = fullscreenEventHook
    <+> focusFollow

-- Toggle follow Mouse
-- from: http://www.haskell.org/haskellwiki/Xmonad/Config_archive/adamvo's_xmonad.hs
-- A nice little example of extensiblestate
newtype FocusFollow = FocusFollow {getFocusFollow :: Bool } deriving (Typeable,Read,Show)
instance ExtensionClass FocusFollow where
    initialValue = FocusFollow True
    extensionType = PersistentExtension

-- this eventHook is the same as from xmonad for handling crossing events
focusFollow e@(CrossingEvent {ev_window=w, ev_event_type=t})
        | t == enterNotify, ev_mode e == notifyNormal =
    whenX (XS.gets getFocusFollow) (focus w) >> return (All True)
focusFollow _ = return (All True)

toggleFF = XS.modify $ FocusFollow . not . getFocusFollow

------------------------------------------------------------------------
-- Startup hook:
myStartupHook :: X ()
myStartupHook = do
    setWMName "LG3D"
    spawn "killall unclutter; unclutter"

------------------------------------------------------------------------
-- General
myWorkspaces = ["1","2","3","4","5","6","7","web","9"]

myConfig xmproc = withUrgencyHook NoUrgencyHook $
    defaultConfig {
        terminal             = "urxvtc"
        , focusFollowsMouse  = False -- see: focusFollow
        , borderWidth        = 2
        -- , modMask            = mod4Mask
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
            , ppSort    = (. namedScratchpadFilterOutWorkspace) <$>
                ppSort defaultPP
            , ppTitle   = (" " ++) . xmobarColor "#ee9a00" ""
            , ppVisible = xmobarColor "#ee9a00" ""
            } >> updatePointer (0.5,0.5) (0.1,0.1)
        }
    
------------------------------------------------------------------------
-- Now run xmonad
main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
    xmonad $ myConfig xmproc
