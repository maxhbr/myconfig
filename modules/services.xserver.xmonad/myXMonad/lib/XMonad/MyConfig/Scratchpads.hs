-- Copyright 2017-2018 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
module XMonad.MyConfig.Scratchpads
       ( applyMyScratchpads
       , scratchpadPPSort)
       where

import           XMonad
import           XMonad.Util.EZConfig (additionalKeys)
import           XMonad.Util.NamedScratchpad ( NamedScratchpad(..)
                                             , customFloating
                                             , namedScratchpadAction
                                             , namedScratchpadFilterOutWorkspace
                                             , namedScratchpadManageHook )
import           XMonad.Hooks.DynamicLog ( def, PP(..))
import qualified XMonad.StackSet as W

import XMonad.MyConfig.Common

applyMyScratchpads :: XConfig a -> XConfig a
applyMyScratchpads c = let

    -- Scratchpads
    --
    scratchpads :: [NamedScratchpad]
    scratchpads =
      let
        mkTermCmd :: String -> String -> String
        mkTermCmd name cmd = "SHLVL=0 " ++ terminal c ++ " -n " ++ name ++ " -e " ++ cmd
        mkEmacsCmd :: String -> String -> String
        mkEmacsCmd name "" = emacsCMD ++ " -name " ++ name
        mkEmacsCmd name cmd = emacsCMD ++ " -name " ++ name ++ " -e \"" ++ cmd ++ "\""
      in
        [ NS "scratchpad" (mkTermCmd "Scratchpad" (pathToXmonadBins ++ "tmux-scratch.sh"))
            (resource =? "Scratchpad")
            (customFloating $ W.RationalRect (1/12) (1/10) (5/6) (4/5))
       , NS "ScratchMu4e" (mkEmacsCmd "ScratchMu4e" "")
           (resource =? "ScratchMu4e")
           (customFloating $ W.RationalRect (1/24) (3/20) (4/6) (3/5))
       , NS "ScratchMutt" (mkTermCmd "ScratchMutt" (bashCMD ++ " -c \"~/bin/mailclient.sh\""))
           (resource =? "ScratchMutt")
           (customFloating $ W.RationalRect (1/24) (3/20) (5/6) (4/5))
       , NS "ScratchHtop" (mkTermCmd "ScratchHtop" (bashCMD ++ " -c " ++ htopCMD))
           (resource =? "ScratchHtop")
           (customFloating $ W.RationalRect (1/10) (1/10) (4/5) (4/5))
       , NS "ScratchNMTUI" (mkTermCmd "ScratchHtop" (bashCMD ++ " -c " ++ htopCMD))
           (resource =? "ScratchNMTUI")
           (customFloating $ W.RationalRect (1/10) (1/10) (4/5) (4/5))
       ]

    scratchpadKBs = map (\(k,d) -> (k, namedScratchpadAction scratchpads d))
                        [ ((m__, xK_minus), "scratchpad")
                        , ((ms_, xK_i    ), "ScratchMu4e")
                        , ((msc, xK_i    ), "ScratchMutt")
                        ]

    scratchpadHook = namedScratchpadManageHook scratchpads

  in c { manageHook = manageHook c <+> scratchpadHook
       } `additionalKeys` mapToWithModM c scratchpadKBs

scratchpadPPSort = (. namedScratchpadFilterOutWorkspace) <$> ppSort def
