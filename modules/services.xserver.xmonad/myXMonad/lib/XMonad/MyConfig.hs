-- Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
{-# OPTIONS_GHC -W -fwarn-unused-imports -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module XMonad.MyConfig
    ( runMyConfig
    , composeMyConfig
    ) where

import           System.Environment ( getExecutablePath, getArgs, )
import           XMonad

import           XMonad.Util.Replace ( replace )

--------------------------------------------------------------------------------
-- MyConfig
import XMonad.MyConfig.Core ( coreConfig )
import XMonad.MyConfig.MyManageHookLayer ( applyMyManageHook )
import XMonad.MyConfig.Scratchpads ( applyMyScratchpads )
import XMonad.MyConfig.ToggleFollowFocus ( applyMyFollowFocus )
import XMonad.MyConfig.Notify ( applyMyUrgencyHook )
import XMonad.MyConfig.MyLayoutLayer ( applyMyLayoutModifications )
import XMonad.MyConfig.MyLogHookLayer ( applyMyLogHook, runXmobar )
import XMonad.Hooks.EwmhDesktops (ewmh)

runMyConfig :: IO ()
runMyConfig = do
  runXmobar

  executablePath <- getExecutablePath
  putStrLn ("xmonad is: " ++ executablePath)
  xmonad composeMyConfig

composeMyConfig = let
  layers :: (LayoutClass a Window) => [XConfig a -> XConfig a]
  layers = [ applyMyLayoutModifications
           , applyMyManageHook
           , applyMyUrgencyHook
           , applyMyScratchpads
           , applyMyFollowFocus
           , applyMyLogHook
           , ewmh -- EWMH should prevent input grab without focus grab:
                  -- see: https://github.com/xmonad/xmonad/issues/45#issuecomment-442064582
                  --   Use EWMH handling, because some programs grab input focus without window focus. With EWMH enabled, this won't happenj
           ]
  in foldl (\ c f -> f c) coreConfig layers
