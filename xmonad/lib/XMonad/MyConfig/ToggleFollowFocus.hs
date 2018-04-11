-- Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
-- stolen from: https://wiki.haskell.org/Xmonad/Config_archive/adamvo's_xmonad.hs
module XMonad.MyConfig.ToggleFollowFocus
       ( focusFollow
       , updatePointerIfFollowFoucs
       , toggleFF
       -- , ((m__,  xK_s      ), toggleFF) -- toggle mouse follow focus
       ) where
import           XMonad
import qualified XMonad.Util.ExtensibleState as XS
import           XMonad.Actions.UpdatePointer ( updatePointer )
import           Data.Monoid

-- Toggle follow Mouse
-- from: http://www.haskell.org/haskellwiki/Xmonad/Config_archive/adamvo's_xmonad.hs
-- A nice little example of extensiblestate
newtype FocusFollow = FocusFollow {getFocusFollow :: Bool} deriving (Typeable,Read,Show)
instance ExtensionClass FocusFollow where
    initialValue = FocusFollow True
    extensionType = PersistentExtension

-- this eventHook is the same as from xmonad for handling crossing events
focusFollow e@(CrossingEvent {ev_window=w, ev_event_type=t})
        | t == enterNotify, ev_mode e == notifyNormal =
    whenX (XS.gets getFocusFollow) (focus w) >> return (All True)
focusFollow _ = return (All True)

-- updatePointerIfFollowFoucs
updatePointerIfFollowFoucs = whenX (XS.gets getFocusFollow) $
  updatePointer (0.5,0.5) (0.5,0.5)

toggleFF = XS.modify $ FocusFollow . not . getFocusFollow
