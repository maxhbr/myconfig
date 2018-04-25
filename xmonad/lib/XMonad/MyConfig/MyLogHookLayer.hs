-- Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
{-# LANGUAGE CPP #-}
module XMonad.MyConfig.MyLogHookLayer
    ( applyMyLogHook
    ) where

import           System.IO ( hPutStrLn )
import           XMonad
import           XMonad.Hooks.DynamicLog ( dynamicLogWithPP
                                         , PP(..)
                                         , xmobarColor
                                         , wrap )

import XMonad.MyConfig.Scratchpads ( scratchpadPPSort )

applyMyLogHook xmproc c =
  let
    maincolor = focusedBorderColor c
    myXmobarPP = def { ppOutput  = hPutStrLn xmproc . shortenStatus
                     , ppCurrent = xmobarColor maincolor "" . wrap "<" ">"
                     , ppSort    = scratchpadPPSort
                     , ppTitle   = (" " ++) . xmobarColor maincolor ""
                     , ppVisible = xmobarColor maincolor ""
                     }
    myLogHook = dynamicLogWithPP myXmobarPP
  in c { logHook = myLogHook }

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

-- shortenStatus :: String -> String
-- shortenStatus = unwords . map shortenPaths . words

shortenStatus :: String -> String
shortenStatus = shortenStatus' "" ""

shortenStatus' :: String -- output
               -> String -- prefix of last word
               -> String -- input
               -> String
shortenStatus' r w ""              = r ++ shortenPaths w
shortenStatus' r w ('\\':(' ':is)) = shortenStatus' r (w ++"\\ ") is
shortenStatus' r w (' ':is)        = shortenStatus' (r ++ shortenPaths w ++ " ") "" is
shortenStatus' r w (i:is)          = shortenStatus' r (w++[i]) is
