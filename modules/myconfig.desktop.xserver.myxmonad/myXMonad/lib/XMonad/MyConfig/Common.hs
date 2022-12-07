-- Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
module XMonad.MyConfig.Common
    ( m__, ms_, m_c, msc, m4m
    , applyMyKBs, applyMyKBs'
    , mkTermCmd, mkTermBashCmd
    , mapToWithModM
    , module X
    ) where
import           XMonad
import           XMonad.Util.EZConfig (additionalKeys)
import           XMonad.MyConfig.Variables as X

mapToWithModM :: XConfig a -> [((KeyMask -> KeyMask, KeySym), X ())] -> [((KeyMask, KeySym), X ())]
mapToWithModM conf = map (\((m,k),v) -> ((m (modMask conf),k),v))
applyMyKBs :: [((KeyMask -> KeyMask, KeySym), X ())] -> XConfig a -> XConfig a
applyMyKBs myKBs conf = additionalKeys conf $ mapToWithModM conf myKBs
applyMyKBs' :: (XConfig a -> [((KeyMask -> KeyMask, KeySym), X ())]) -> XConfig a -> XConfig a
applyMyKBs' myKBs conf = additionalKeys conf $ mapToWithModM conf (myKBs conf)

mkTermCmd :: XConfig a -> String -> String -> String
mkTermCmd c "" cmd = "SHLVL=0 " ++ terminalNoTmuxCMD ++ " -e " ++ cmd
mkTermCmd c name cmd = "SHLVL=0 " ++ terminalNoTmuxCMD ++ " -n " ++ name ++ " -e " ++ cmd
mkTermBashCmd :: XConfig a -> String -> String -> String
mkTermBashCmd c name cmd = mkTermCmd c name (bashCMD ++ " -c " ++ cmd)

 {-
/---- meta
|/--- shift
||/-- control
|||
vvv -}
m__, ms_, m_c, msc, m4m :: KeyMask -> KeyMask
m__ = id
ms_ m = m .|. shiftMask
m_c m = m .|. controlMask
msc m = m .|. shiftMask .|. controlMask
m4m   = const mod4Mask

