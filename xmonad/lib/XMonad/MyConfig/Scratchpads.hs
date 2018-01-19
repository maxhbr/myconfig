module XMonad.MyConfig.Scratchpads
       ( scratchpadHook
       , scratchpadKBs
       , scratchpadPPSort)
       where

import XMonad
import           XMonad.Util.NamedScratchpad ( NamedScratchpad(..)
                                             , customFloating , nonFloating
                                             , namedScratchpadAction
                                             , namedScratchpadFilterOutWorkspace
                                             , namedScratchpadManageHook )
import           XMonad.Hooks.DynamicLog ( def, PP(..))
import qualified XMonad.StackSet             as W

import XMonad.MyConfig.Common

-- Scratchpads
--
scratchpads :: [NamedScratchpad]
scratchpads =
    [ NS "scratchpad" "SHLVL=0 urxvtc -name Scratchpad -e ~/.xmonad/bin/tmux-scratch.sh"
        (resource =? "Scratchpad")
        (customFloating $ W.RationalRect (1/12) (1/10) (5/6) (4/5))
    , NS "ScratchWeb" "Chromium" (resource =? "Chromium") nonFloating
        {-(customFloating $ W.RationalRect (1/64) (3/128) (31/32) (31/32))-}
    , NS "ncmpcpp" "SHLVL=0 urxvtc -name Ncmpcpp -e ncmpcpp"
        (resource =? "Ncmpcpp")
        (customFloating $ W.RationalRect (1/2) (1/5) (1/2) (4/5))
    , NS "notepad" "SHLVL=0 /usr/bin/emacsclient -a \"\" -nc ~/Sync/org/index.org"
        (resource =? "Notepad")
        (customFloating $ W.RationalRect (4/12) (3/20) (7/12) (4/5))
   , NS "ScratchMutt" "SHLVL=0 urxvtc -name ScratchMutt -e bash -c \"~/bin/mailclient.sh\""
       (resource =? "ScratchMutt")
       (customFloating $ W.RationalRect (1/24) (3/20) (5/6) (4/5))
   , NS "ScratchAlsa" "SHLVL=0 urxvtc -name ScratchAlsa -e bash -c alsamixer"
       (resource =? "ScratchAlsa")
       (customFloating $ W.RationalRect (1/24) (3/20) (5/6) (4/5))
   , NS "ScratchHtop" "SHLVL=0 urxvtc -name ScratchHtop -e bash -c htop"
       (resource =? "ScratchHtop")
       (customFloating $ W.RationalRect (1/10) (1/10) (4/5) (4/5))
   , NS "ScratchNMTUI" "SHLVL=0 urxvtc -name ScratchHtop -e bash -c htop"
       (resource =? "ScratchNMTUI")
       (customFloating $ W.RationalRect (1/10) (1/10) (4/5) (4/5)) ]

scratchpadKBs =
  map (\(k,d) -> (k, namedScratchpadAction scratchpads d))
  [ ((m__, xK_minus), "scratchpad")
  , ((m__, xK_i    ), "ScratchWeb")
  , ((ms_, xK_i    ), "ScratchMutt")
  , ((m__, xK_n    ), "notepad")
  , ((ms_, 0xf6    ), "ncmpcpp")]

scratchpadHook = namedScratchpadManageHook scratchpads

scratchpadPPSort = (. namedScratchpadFilterOutWorkspace) <$> ppSort def
