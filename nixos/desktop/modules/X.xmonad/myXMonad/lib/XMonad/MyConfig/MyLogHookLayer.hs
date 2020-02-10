-- Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module XMonad.MyConfig.MyLogHookLayer
    ( applyMyLogHook
    , runXmobar
    ) where

import           XMonad
import           XMonad.Util.Run ( safeSpawn )
import           XMonad.Hooks.DynamicLog ( dynamicLogString, xmonadPropLog
                                         , PP(..)
                                         , xmobarColor
                                         , wrap )

import XMonad.MyConfig.Common
import XMonad.MyConfig.Scratchpads ( scratchpadPPSort )

runXmobar :: IO ()
runXmobar = safeSpawn xmobarCMD []

applyMyLogHook c =
  let
    maincolor = focusedBorderColor c
    myXmobarPP = def { ppCurrent = xmobarColor maincolor "" . wrap "<" ">"
                     , ppSort    = scratchpadPPSort
                     , ppTitle   = (" " ++) . shortenStatus . xmobarColor maincolor ""
                     , ppVisible = xmobarColor maincolor ""
                     }

    myLogHook :: X ()
    myLogHook = dynamicLogString myXmobarPP >>= xmonadPropLog
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
