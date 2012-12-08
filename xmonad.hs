-- xmonad config file for xmobar, dmenu
-- Last modified: Sa Dez 08, 2012  05:18

import XMonad
import XMonad.ManageHook
import Data.Monoid
import System.Exit
import System.IO

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers

import XMonad.Util.EZConfig
--import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)

import XMonad.Actions.CycleWS
--import XMonad.Actions.GridSelect
--import XMonad.Actions.SpawnOn

import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Magnifier
import XMonad.Layout.Named
import XMonad.Layout.LayoutHints
import XMonad.Layout.ResizableTile

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

--import Graphics.X11.ExtraTypes.XF86

-- The preferred terminal program
--
myTerminal = "urxvtc" -- -e tmux"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Width of the window border in pixels.
myBorderWidth :: Dimension
myBorderWidth = 2

-- modMask
--
myModMask = mod4Mask

------------------------------------------------------------
-- The number of workspaces (virtual screens) and their names.
--
myWorkspaces = ["1","2","3","4","5","6","mail","web","chat"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#333333"      -- #dddddd
myFocusedBorderColor = "#dd0000"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--{{{
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    , ((modm,               xK_p     ), spawn "dmenu_run")
    , ((modm,               xK_o     ), spawn "emelfm2")
    , ((modm,               xK_i     ), spawn "dwb")
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink and Expand
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modm .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm,               xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm,               xK_period), sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

     ,((modm,               xK_b     ), sendMessage ToggleStruts)

    -- Restart xmonad
    , ((modm,                xK_q    ), spawn "xmonad --recompile; xmonad --restart")

    , ((modm .|. shiftMask,  xK_F11  ),  spawn "systemctl suspend") --suspend
    , ((modm .|. shiftMask,  xK_F11  ),  spawn "systemctl reboot") --reboot
    , ((modm .|. shiftMask,  xK_F12  ),  spawn "systemctl poweroff") --shutdown

    -- toggle touchpad
    , ((0,                  0x1008ffa9), spawn "synclient TouchpadOff=$(synclient -l | grep -c 'TouchpadOff.*=.*0')")

    -- screensaver
    , ((modm .|. shiftMask,  xK_y    ), spawn "xbacklight -set 0; xscreensaver-command -lock")

    --invert Colors
    , ((modm,                xK_Home ), spawn "xcalib -i -a")

    -- screenshot
    , ((0,                   xK_Print), spawn "scrot screen_%Y-%m-%d_%H-%M-%S.png -d 1")

    --volume controls
    , ((0,                  0x1008ff12), spawn "/home/hubi/.xmonad/myvolume.sh m")
    , ((0,                  0x1008ff11), spawn "/home/hubi/.xmonad/myvolume.sh -")
    , ((0,                  0x1008ff13), spawn "/home/hubi/.xmonad/myvolume.sh +")

    -- alternative zu anderem starter
    , ((modm,                xK_x     ), shellPrompt defaultXPConfig)
    --, ((modm , xK_x), sshPrompt defaultXPConfig)

     -- toggle mouse
    , ((modm,                xK_s     ), spawn "/home/hubi/.xmonad/togglemouse.sh silent off")
    , ((modm .|. shiftMask,  xK_s     ), spawn "/home/hubi/.xmonad/togglemouse.sh")

    -- check for dock, set up desktop
    , ((modm .|. shiftMask, xK_d) , spawn "/home/hubi/bin/mydock.sh")

    -- CycleWS setup
    , ((modm,                xK_Down  ), moveTo Next NonEmptyWS)
    , ((modm,                xK_Up    ), moveTo Prev NonEmptyWS)
    , ((modm .|. shiftMask,  xK_Down  ), shiftToNext >> nextWS)
    , ((modm .|. shiftMask,  xK_Up    ), shiftToPrev >> prevWS)
    , ((modm,                xK_Right ), nextScreen)
    , ((modm,                xK_Left  ), prevScreen)
    , ((modm .|. shiftMask,  xK_Right ), shiftNextScreen)
    , ((modm .|. shiftMask,  xK_Left  ), shiftPrevScreen)
    , ((modm,                xK_y     ), toggleWS)

    , ((modm .|. shiftMask,  xK_minus ), namedScratchpadAction scratchpads "scratchpad")
    , ((modm,                xK_g     ), namedScratchpadAction scratchpads "ScratchGvim")
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm,           k        ), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modm,           key      ), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
--}}}
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--{{{
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
--}}}
------------------------------------------------------------------------
-- Layouts:
--{{{
myLayout = avoidStruts $ smartBorders
    (tiled
    ||| mag
    ||| full
    ||| stb
    )  -- Mirror tiled
  where
    --tiled   = named "tiled" $ Tall  nmaster delta ratio
    tiled   = named " " $ ResizableTall nmaster delta ratio []
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100
    mag     = named "zoom" $ magnifier (Tall 1 (3/100) (1/2))
    full    = named "full" $ Full
    --stb     = named "tabs" $ simpleTabbedBottom
    stb     = named "tabs" $ tabbedBottom shrinkText myTab

myTab = defaultTheme
    { activeColor         = "black"
    , inactiveColor       = "black"
    , urgentColor         = "yellow"
    , activeBorderColor   = "orange"
    , inactiveBorderColor = "#333333"
    , urgentBorderColor   = "black"
    , activeTextColor     = "orange"
    , inactiveTextColor   = "#666666"
    , urgentTextColor     = "yellow" }
--}}}
------------------------------------------------------------------------
-- Window rules:
--{{{
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
    [ className =? "MPlayer"        --> doFloat
    , className =? "Pidgin"         --> doShift "chat"
    , className =? "Chromium"       --> doShift "web"
    , className =? "Sylpheed"       --> doShift "mail"
    , className =? "Gimp"           --> doShift "4"
    , resource =? "Gimp"            --> doShift "4"
    , className =? "Virtualbox"     --> doFullFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , className =? "Zenity"         --> doCenterFloat] <+> namedScratchpadManageHook scratchpads

-- Scratchpads
--
scratchpads :: [NamedScratchpad]
scratchpads = [
        NS "scratchpad" "urxvt -name Scratchpad" (resource =? "Scratchpad")
            (customFloating $ W.RationalRect (1/12) (1/10) (5/6) (4/5))
        , NS "ScratchGvim" "gvim --role ScratchGvim" (role =? "ScratchGvim") nonFloating
    ] where role = stringProperty "WM_WINDOW_ROLE"
--}}}
------------------------------------------------------------------------
-- Event handling

-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
--myEventHook = mempty
myEventHook = fullscreenEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--myLogHook = return ()
myLogHook = dynamicLog

------------------------------------------------------------------------
-- Startup hook
--
--myStartupHook = return ()
myStartupHook :: X ()
myStartupHook = do
    spawn "/home/hubi/.xmonad/mystartup.sh"
    --spawn "unclutter &"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
{-main = xmonad =<< statusBar myBar myPP toggleStrutsKey defaults-}
main = do
    xmproc <- spawnPipe "xmobar /home/hubi/.xmonad/xmobarrc"
    xmonad $ myConfig xmproc

myConfig xmproc = withUrgencyHook NoUrgencyHook $ defaultConfig {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        manageHook = myManageHook <+> manageDocks
            <+> manageHook defaultConfig,
        layoutHook         = myLayout,
        handleEventHook    = myEventHook,
        logHook            = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
                , ppCurrent = xmobarColor "#ee9a00" "" . wrap "<" ">"
                , ppSort = fmap (.namedScratchpadFilterOutWorkspace)
                           $ ppSort defaultPP
                , ppTitle = (" " ++) . xmobarColor "#ee9a00" ""
                , ppVisible = xmobarColor "#ee9a00" ""
            },
        startupHook        = myStartupHook
    }
