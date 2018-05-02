-- Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
{-# OPTIONS_GHC -W -fwarn-unused-imports -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module XMonad.MyConfig
    ( runMyConfig
    ) where

import           System.Environment ( getExecutablePath )

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

runMyConfig = do
  xmproc <- spawnPipe (xmobarCMD ++ " " ++ pathToXmobarConfig)
  executablePath <- getExecutablePath
  xmonad $ myConfig xmproc executablePath

myConfig xmproc executablePath = let
  layers :: (LayoutClass a Window) => [XConfig a -> XConfig a]
  layers = [ applyMyLayoutModifications
           , applyMyUrgencyHook
           , applyMyScratchpads
           , applyMyFollowFocus
           , applyMyLogHook xmproc
           ]
  in foldl (\ c f -> f c) (coreConfig executablePath) layers
