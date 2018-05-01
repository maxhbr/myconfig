-- Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
{-# OPTIONS_GHC -W -fwarn-unused-imports -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module XMonad.MyConfig
    ( runMyConfig
    ) where

import           XMonad
import           XMonad.Util.Run ( spawnPipe )

--------------------------------------------------------------------------------
-- MyConfig
import XMonad.MyConfig.Core ( coreConfig )
import XMonad.MyConfig.Variables
import XMonad.MyConfig.Scratchpads ( applyMyScratchpads )
import XMonad.MyConfig.ToggleFollowFocus ( applyMyFollowFocus )
import XMonad.MyConfig.Notify ( applyMyUrgencyHook )
import XMonad.MyConfig.MyLayoutLayer ( applyMyLayoutModifications )
import XMonad.MyConfig.MyLogHookLayer ( applyMyLogHook )
import XMonad.MyConfig.MyAutospawnLayer ( applyMyAutospawnLayer )

runMyConfig = do
  xmproc <- spawnPipe (xmobarCMD ++ " " ++ pathToXmobarConfig)
  xmonad $ myConfig xmproc

myConfig xmproc = let
  layers :: (LayoutClass a Window) => [XConfig a -> XConfig a]
  layers = [ applyMyLayoutModifications
           , applyMyUrgencyHook
           , applyMyScratchpads
           , applyMyFollowFocus
           , applyMyLogHook xmproc
           , applyMyAutospawnLayer
           ]
  in foldl (\ c f -> f c) coreConfig layers
