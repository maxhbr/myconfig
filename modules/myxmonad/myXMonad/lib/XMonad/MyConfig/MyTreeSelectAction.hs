-- Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module XMonad.MyConfig.MyTreeSelectAction
    ( applyTreeselectLayer
    ) where

import           XMonad
import           XMonad.Actions.TreeSelect ( treeselectAction, tsDefaultConfig, TSConfig (..)
                                           , TSNode (..) )
import           Data.Tree ( Tree (..) )

import           XMonad.MyConfig.Common
import           XMonad.MyConfig.Variables

applyTreeselectLayer c = let 

    myTreeConf :: TSConfig a
    myTreeConf = tsDefaultConfig

    myTreeselectAction :: X () 
    myTreeselectAction = treeselectAction myTreeConf
        [ Node (TSNode "GUI" "" (return ())) 
            [ Node (TSNode "Firefox" "" (spawn firefoxCMD)) []          
            , Node (TSNode "chromium-browser" "" (spawn browserCMD)) []          
            ]
        , Node (TSNode "Audio" "" (spawn pavucontrolCMD)) []
        , Node (TSNode "System Debug" "spawn htop" (spawn (mkTermBashCmd c "ScratchHtop" htopCMD)))
            (map (\cmd -> Node (TSNode cmd ("spawn " ++ cmd) (spawn (mkTermBashCmd c "" cmd))) [])
                [ "htop"
                , "sudo iftop"
                , "sudo iptraf-ng"
                , "sudo iotop"
                , "bmon"
                , "nmon"
                , "atop"
                , "s-tui"
                , "usbtop" 
                , "mtr 1.1.1.1"
                ])
        ]

    in applyMyKBs [((m_c, xK_p), myTreeselectAction)] c
