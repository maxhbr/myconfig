-- Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
{-# LANGUAGE CPP #-}
module XMonad.MyConfig.MyLayoutLayer
    ( applyMyLayoutModifications
    , myLayout
    ) where

import           Data.Ratio ((%))

import XMonad
import           XMonad.Util.EZConfig (additionalKeys)

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
                                        , WSType( NonEmptyWS ) )
import           XMonad.Actions.WindowGo ( runOrRaiseNext, raiseNext )

--------------------------------------------------------------------------------
-- Hooks
import           XMonad.Hooks.DynamicLog ( dynamicLogWithPP
                                         , PP(..)
                                         , xmobarColor
                                         , wrap )
import           XMonad.Hooks.EwmhDesktops ( fullscreenEventHook )
import           XMonad.Hooks.ManageDocks ( docks
                                          , avoidStrutsOn
                                          , ToggleStruts(..) )
import           XMonad.Hooks.ManageHelpers ( doCenterFloat )

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

import XMonad.MyConfig.Common
import XMonad.MyConfig.Notify (popupCurDesktop)

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

layoutKBs conf =
  mapToWithModM conf $
  [ ((m__, xK_space ), sendMessage NextLayout)
  , ((ms_, xK_x     ), sendMessage $ Toggle MIRROR)
  , ((m__, xK_f     ), sendMessage $ Toggle FULL)
  -- , ((ms_, xK_space ), setLayout $ myLayout)
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
       , ((m__, 0xfc), runOrRaiseNext "ec" (className =? "Emacs"))
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

applyMyLayoutModifications :: XConfig a -> XConfig a
applyMyLayoutModifications c = let
  addLayoutkeys :: XConfig a -> XConfig a
  addLayoutkeys c = additionalKeys c (layoutKBs c)
  in docks $
     addLayoutkeys $
     c { workspaces      = myWorkspaces
       , handleEventHook = fullscreenEventHook <+> (handleEventHook c)
       , mouseBindings   = myMouseBindings
       }
