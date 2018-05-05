-- Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
{-# OPTIONS_GHC -W -fwarn-unused-imports -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module XMonad.MyConfig
    ( runMyConfig
    , composeMyConfig
    ) where

import           System.Environment ( getExecutablePath )
import           GHC.IO.Handle ( Handle (..) )
import           System.FilePath ( FilePath (..) )

import           XMonad

--------------------------------------------------------------------------------
-- MyConfig
import XMonad.MyConfig.Variables

import XMonad.MyConfig.Core ( coreConfig, applyMyRestartKBs )
import XMonad.MyConfig.MyManageHookLayer ( applyMyManageHook )
import XMonad.MyConfig.Scratchpads ( applyMyScratchpads )
import XMonad.MyConfig.ToggleFollowFocus ( applyMyFollowFocus )
import XMonad.MyConfig.Notify ( applyMyUrgencyHook )
import XMonad.MyConfig.MyLayoutLayer ( applyMyLayoutModifications )
import XMonad.MyConfig.MyLogHookLayer ( getXMProcs , applyMyLogHook )
import XMonad.Layout.IndependentScreens

runMyConfig :: IO ()
runMyConfig = do
  xmprocs <-getXMProcs
  executablePath <- getExecutablePath
  xmonad $ composeMyConfig xmprocs executablePath

-- composeMyConfig :: (LayoutClass a Window)
--                 => [Handle] -> FilePath -> XConfig a
composeMyConfig xmprocs executablePath = let
  layers :: (LayoutClass a Window) => [XConfig a -> XConfig a]
  layers = [ applyMyLayoutModifications
           , applyMyRestartKBs executablePath
           , applyMyManageHook
           , applyMyUrgencyHook
           , applyMyScratchpads
           , applyMyFollowFocus
           , applyMyLogHook xmprocs
           ]
  in foldl (\ c f -> f c) coreConfig layers
