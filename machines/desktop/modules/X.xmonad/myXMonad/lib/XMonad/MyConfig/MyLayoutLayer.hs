-- Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
{-# LANGUAGE CPP #-}
module XMonad.MyConfig.MyLayoutLayer
    ( applyMyLayoutModifications
    , myLayout
    ) where

import           Data.Ratio ((%))
import           Data.Maybe ( isJust )

import           XMonad
import           XMonad.StackSet ( stack, tag )

--------------------------------------------------------------------------------
-- Util
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
                                        , WSType( WSIs ) )
import           XMonad.Actions.Minimize ( minimizeWindow, withLastMinimized, maximizeWindowAndFocus )

--------------------------------------------------------------------------------
-- Hooks
import           XMonad.Hooks.EwmhDesktops ( fullscreenEventHook )
import           XMonad.Hooks.ManageDocks ( docks
                                          , avoidStrutsOn
                                          , ToggleStruts(..) )

--------------------------------------------------------------------------------
-- Layouts
import           XMonad.Layout.BoringWindows( boringAuto
                                            , focusDown )
import           XMonad.Layout.Gaps (gaps, GapMessage(ToggleGaps))
import           XMonad.Layout.Named ( named )
import           XMonad.Layout.NoBorders ( smartBorders )
import           XMonad.Layout.Minimize ( minimize )
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.PerScreen (ifWider)
import           XMonad.Layout.PerWorkspace ( modWorkspaces )
import           XMonad.Layout.ResizableTile ( ResizableTall(ResizableTall)
                                             , MirrorResize ( MirrorShrink
                                                            , MirrorExpand ) )
import           XMonad.Layout.Spacing ( spacingRaw, Border(..), toggleWindowSpacingEnabled, toggleScreenSpacingEnabled)
import           XMonad.Layout.TwoPane ( TwoPane( TwoPane ) )
import           XMonad.Layout.IM -- (withIM)

import           XMonad.Layout.IfMax

--------------------------------------------------------------------------------
-- misc
import qualified Data.Map                    as M
import qualified XMonad.StackSet             as W

import XMonad.MyConfig.Common
import XMonad.MyConfig.Notify (popupCurDesktop)


myCoreWorkspaces, myWorkspaces :: [String]
myCoreWorkspaces = "web" : map show [9..10]
myWorkspaces = map show [1..7] ++ myCoreWorkspaces ++ ["vbox", "media"] ++ map show [13..20] ++ ["NSP"]
applyMyLayoutModifications :: XConfig a -> XConfig a
applyMyLayoutModifications c = let
  addLayoutkeys :: XConfig a -> XConfig a
  addLayoutkeys = applyMyKBs' layoutKBs
  in docks $
     addLayoutkeys $
     c { workspaces      = myWorkspaces
       , handleEventHook = fullscreenEventHook <+> handleEventHook c
       , mouseBindings   = myMouseBindings }

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
    hdGapping = 150
    wqhdGapping = (2560 - 1920) `div` 2 - wqhdSpacing + baseSpacing
    myUprightGapping l = ifWider 1440 l $
                         ifWider 1439 (gaps [(U,wqhdGapping), (D,wqhdGapping)] l) l
    myUprightMirroring l = ifWider 1440 l $
                           ifWider 1439 (Mirror l) l

    mySpacing l = let
        spacing w = spacingRaw False (Border 0 wIntegral 0 wIntegral) True (Border wIntegral 0 wIntegral 0) True
          where
            wIntegral =  fromIntegral w
      in ifWider 1920 (spacing wqhdSpacing l) $
         ifWider 1919 (spacing baseSpacing l) $
         ifWider 1439 (spacing wqhdSpacing l)
         l
    full      = named "=" $
                mySpacing $
                ifWider 1920 (gaps [(L,wqhdGapping), (R,wqhdGapping)] Full) $
                ifWider 1919 (gaps [(L,hdGapping), (R,hdGapping)] Full) $
                myUprightGapping Full
    tiled     = named " " $
                mySpacing $
                minimize $
                myUprightGapping $
                myUprightMirroring $
                ResizableTall 1 (3/100) (1/2) []
    dtb       = named "%" $
                mySpacing $
                minimize $
                myUprightGapping $
                myUprightMirroring $
                TwoPane (3/100) (1/2)

layoutKBs conf =
  [ ((m__, xK_space ), sendMessage NextLayout)
  , ((ms_, xK_x     ), sendMessage $ Toggle MIRROR)
  , ((m__, xK_f     ), sendMessage $ Toggle FULL)
  , ((ms_, xK_f     ), do
        sendMessage ToggleGaps
        toggleScreenSpacingEnabled
        toggleWindowSpacingEnabled)
  , ((m__, xK_m     ), withFocused minimizeWindow)
  , ((ms_, xK_m     ), withLastMinimized maximizeWindowAndFocus)

  , ((m__, xK_t     ), withFocused $ windows . W.sink) -- Push window back into tiling
  , ((m__, xK_comma ), sendMessage (IncMasterN 1))
  , ((m__, xK_period), sendMessage (IncMasterN (-1)))

  -- Shrink and Expand
  , ((m__, xK_h     ), sendMessage Shrink)
  , ((m__, xK_l     ), sendMessage Expand)
  , ((ms_, xK_h     ), sendMessage MirrorShrink)
  , ((ms_, xK_l     ), sendMessage MirrorExpand)

  , ((m__, xK_b     ), sendMessage ToggleStruts) ]
  ++ cycleWSKBs
  ++ switchWorkspaceKBs
  ++ focusKBs
  -- ++ switchPhysicalKBs
  -- ++ combineTwoKBs
  -- ++ subLayoutKBs
  where
    switchWorkspaceKBs =
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      [((m, k), f i)
          | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
          , (f, m) <- [ (\i -> windows (W.greedyView i) >> popupCurDesktop, m__)
                      , (windows . W.shift, ms_) ]]
      ++ [((msc, xK_m    ), (windows . W.shift) "NSP" )]
    focusKBs =
      [ ((ms_, xK_Tab   ), focusDown)
#if 0
      , ((m__, xK_Tab   ), windows W.focusDown)
      , ((m_c, xK_Tab   ), windows W.focusUp >> windows W.shiftMaster)
#else
      , ((m__, xK_Tab   ), windows W.focusUp >> windows W.shiftMaster)
      , ((m_c, xK_Tab   ), windows W.focusDown)
#endif

      , ((m__, xK_j     ), windows W.focusDown)
      , ((m__, xK_k     ), windows W.focusUp)
      , ((ms_, xK_j     ), windows W.swapDown)
      , ((ms_, xK_k     ), windows W.swapUp)
      ]
    cycleWSKBs = let
        nonEmptyNonMinorWS = WSIs $ do
           let ne = isJust . stack -- equals NonEmptyWS in XMonad.Actions.CycleWS
           let mw = not . (`elem` myCoreWorkspaces) . tag
           return (\w -> ne w && mw w)
      in map (\(a,b) -> (a,b >> popupCurDesktop))
        [ ((m__, xK_Down ), moveTo Next nonEmptyNonMinorWS) -- HiddenNonEmptyWS
        , ((m__, xK_Up   ), moveTo Prev nonEmptyNonMinorWS) -- HiddenNonEmptyWS
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
      -- switchPhysicalKBs =
      --   -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
      --   -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
      --   [((m .|. m__, k), screenWorkspace sc >>= flip whenJust (windows . f))
      --       | (k, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      --       , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
      -- combineTwoKBs =
      --   [((msc, xK_l ), sendMessage $ Move L)]
      -- subLayoutKBs =
      --   map (\(k,v) -> ((m_c, k), sendMessage $ pullGroup v))
      --     [(xK_h,L),(xK_l,R),(xK_k,U),(xK_j,D)]
      --     ++ [ ((m_c, xK_m), withFocused (sendMessage . MergeAll))
      --        , ((m_c, xK_u), withFocused (sendMessage . UnMerge))]

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
