-- Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
{-# LANGUAGE CPP #-}
module XMonad.MyConfig.MyMiscKBs
    ( restartXmonadKBs
    , backlightControlKBs
    , volumeControlKBs
    ) where

import           XMonad
import           XMonad.Util.Run ( runProcessWithInput )
import           XMonad.Actions.GridSelect

import XMonad.MyConfig.Common
import XMonad.MyConfig.Variables
import XMonad.MyConfig.Notify

restartXmonadKBs :: FilePath -> [((KeyMask -> KeyMask, KeySym), X ())]
restartXmonadKBs executablePath =
  [ ((m__, xK_q     ), spawn (executablePath ++ " --restart"))
  , ((ms_, xK_q     ), spawn (executablePath ++ " --recompile && sleep 0.1 && " ++ executablePath ++ " --restart")) ]

backlightControlKBs, volumeControlKBs :: [((KeyMask -> KeyMask, KeySym), X ())]
backlightControlKBs = [((m__, xK_F1), spawnSelected def [ "xbacklight =50"
                                                        , "xbacklight =25"
                                                        , "xbacklight +10"
                                                        , "xbacklight =75"
                                                        , "xbacklight -10"
                                                        , "xbacklight =10"
                                                        , "xbacklight =5"
                                                        , "xbacklight +1"
                                                        , "xbacklight =3"
                                                        , "xbacklight =100"
                                                        , "xbacklight =1"
                                                        , "xbacklight -1"
                                                        , "xbacklight =0" ])]

volumeControlKBs =
#if 1
-- pulseaudio
  map (\(k,args) -> ((const 0, k)
                 , runProcessWithInput (pathToXmonadBins ++ "mypamixer.sh") args ""
                   >>= myDefaultPopup . ("V: " ++)
                 ))
    [ (0x1008ff12, ["mute"])
    , (0x1008ff11, ["-10%"])
    , (0x1008ff13, ["+10%"])]
#else
-- alsa
  map (\(k,v) -> ((const 0, k)
                 , runProcessWithInput (pathToXmonadBins ++ "myamixer.sh") v ""
                   >>= myDefaultPopup
                 ))
    [ (0x1008ff12, ["toggle"])
    , (0x1008ff11, ["6dB-"])
    , (0x1008ff13, ["unmute","3dB+"])]
#endif
