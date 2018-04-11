-- Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
-- stolen from: https://wiki.haskell.org/Xmonad/Config_archive/adamvo's_xmonad.hs


-- , ((m__,  xK_s      ), toggleFF) -- toggle mouse follow focus
-- , ((msc,  xK_s      ), toggleUP) -- toggle mouse update pointer

module XMonad.MyConfig.ToggleFollowFocus
       ( focusFollow
       , toggleFF
       , toggleUP
       , updatePointerIfFollowFoucs
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

toggleFF = XS.modify $ FocusFollow . not . getFocusFollow

newtype UpdatePointer = UpdatePointer {getUpdatePointer :: Bool} deriving (Typeable,Read,Show)
instance ExtensionClass UpdatePointer where
    initialValue = UpdatePointer True
    extensionType = PersistentExtension

-- updatePointerIfFollowFoucs
updatePointerIfFollowFoucs = whenX (XS.gets getFocusFollow) $
  whenX (XS.gets getUpdatePointer) $
  updatePointer (0.5,0.5) (0.5,0.5)

toggleUP = XS.modify $ UpdatePointer . not . getUpdatePointer
