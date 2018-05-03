-- Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module XMonad.MyConfig.MyLogHookLayer
    ( getXMProcs
    , applyMyLogHook
    ) where

import           GHC.IO.Handle (Handle ())
import           System.IO ( hPutStrLn )
import           XMonad
import           XMonad.Util.Run ( spawnPipe )
import           XMonad.Hooks.DynamicLog ( dynamicLogWithPP
                                         , PP(..)
                                         , xmobarColor
                                         , wrap )

import XMonad.MyConfig.Common
import XMonad.MyConfig.Scratchpads ( scratchpadPPSort )
import XMonad.Layout.IndependentScreens

getXMProcs :: IO [Handle]
getXMProcs = let
    additinalConfig 0 = [pathToXmobarConfig]
    additinalConfig _ = [pathToXmobarMinConfig]
    mkXmobarCommand (S s) = unwords ([xmobarCMD, "-x", show s] ++ additinalConfig s)
  in do
    nScreens <- countScreens
    mapM (spawnPipe . mkXmobarCommand) [0 .. nScreens-1]

applyMyLogHook xmprocs c =
  let
    maincolor = focusedBorderColor c
    myXmobarPP xmproc s = def { ppOutput  = hPutStrLn xmproc
                              , ppCurrent = xmobarColor maincolor "" . wrap "<" ">"
                              , ppSort    = scratchpadPPSort
                              , ppTitle   = if s == 0
                                            then (" " ++) . shortenStatus . xmobarColor maincolor ""
                                            else (const "")
                              , ppVisible = xmobarColor maincolor ""
                              }

    myLogHook :: X ()
    myLogHook = mapM_ dynamicLogWithPP $ zipWith myXmobarPP xmprocs [0..]
  in c { logHook = myLogHook }

shortenStatus :: String -> String
shortenStatus = let
    shortenPaths :: String -> String
    shortenPaths ('~':('/': p)) = "~/" ++ shortenPaths' "" p
    shortenPaths ('/': p)       = "/" ++ shortenPaths' "" p
    shortenPaths ('[': p)       = '[' : if last p == ']'
                                        then shortenPaths' "" p
                                        else p
    shortenPaths p              = p

    shortenPaths' :: String -- output
                  -> String -- input
                  -> String
    shortenPaths' r p@('<':_) = r ++ p
    shortenPaths' r p@('>':_) = r ++ p
    shortenPaths' r ""        = r
    shortenPaths' r "/"       = r ++ "/"
    shortenPaths' _ ('/':p)   = shortenPaths' ".../" p
    shortenPaths' r (c:p)     = shortenPaths' (r++[c]) p

    shortenStatus' :: String -- output
                   -> String -- prefix of last word
                   -> String -- input
                   -> String
    shortenStatus' r w ""              = r ++ shortenPaths w
    shortenStatus' r w ('\\':(' ':is)) = shortenStatus' r (w ++"\\ ") is
    shortenStatus' r w (' ':is)        = shortenStatus' (r ++ shortenPaths w ++ " ") "" is
    shortenStatus' r w (i:is)          = shortenStatus' r (w++[i]) is
  in shortenStatus' "" ""
