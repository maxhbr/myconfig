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
import           Control.Monad ( when )
import           XMonad

import           XMonad.Util.Replace ( replace )

--------------------------------------------------------------------------------
-- MyConfig
import XMonad.MyConfig.Core ( coreConfig, applyMyRestartKBs )
import XMonad.MyConfig.MyManageHookLayer ( applyMyManageHook )
import XMonad.MyConfig.Scratchpads ( applyMyScratchpads )
import XMonad.MyConfig.ToggleFollowFocus ( applyMyFollowFocus )
import XMonad.MyConfig.Notify ( applyMyUrgencyHook )
import XMonad.MyConfig.MyLayoutLayer ( applyMyLayoutModifications )
import XMonad.MyConfig.MyLogHookLayer ( getXMProcs, applyMyLogHook )

-- runMyConfig :: IO ()
-- runMyConfig = do
--   xmprocs <-getXMProcs
--   executablePath <- getExecutablePath
--   xmonad $ composeMyConfig xmprocs executablePath

runMyConfig :: IO ()
runMyConfig = do
  args <- getArgs

  when ("--myreplace" `elem` args) $ do
    putStrLn "try to replace current window manager ..."
    replace

  xmprocs <- getXMProcs
  executablePath <- getExecutablePath
  putStrLn ("try to launch: " ++ executablePath)
  launch $ composeMyConfig xmprocs executablePath

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
