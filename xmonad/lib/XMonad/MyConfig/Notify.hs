module XMonad.MyConfig.Notify
       ( myUrgencyHook, popupCurDesktop, myDefaultPopup, myAlert )
       where
-- needs `notify-osd` and `libnotify`
-- See: https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
--  and https://wiki.haskell.org/Xmonad/Config_archive/doitan%27s_xmonad.hs
--  and https://github.com/lierdakil/xmonad-config/blob/master/Local/Popup.hs

import Data.Char (isSpace)
import           XMonad
import XMonad.Util.Dzen
-- import XMonad.Config.Prime.Monadic
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows
import qualified XMonad.StackSet as W
import XMonad.Hooks.ServerMode
import Control.Applicative

myUrgencyHook = MyUrgencyHook (myNotify 2)

newtype MyUrgencyHook = MyUrgencyHook { muhPopup :: String -> X () }

instance UrgencyHook MyUrgencyHook where
  urgencyHook MyUrgencyHook { muhPopup = pop } w =
      withWindowSet . (. W.findTag w) . flip whenJust . flash =<< getName w
    where
      flash name index =
        pop (show name ++ " requests your attention on workspace " ++ index)

mkFont s = font $ "xft:inconsolata:pixelsize=" ++ show s ++ ":antialias=true:hinting=true"

myNotify :: Rational -> String -> X ()
myNotify t msg = do
  dzenConfig pc msg
  myAlert msg
  where
    pc = onCurr (vCenter 22) >=> timeout t >=> background "darkgreen" >=> mkFont 18
    background color = addArgs ["-bg", color]

myPopup :: Int -> Rational -> String -> X ()
myPopup width t s = do
  dzenConfig pc s
  -- myAlert s
    where
      pc = onCurr (hCenter width) >=> timeout t >=> mkFont 80

myDefaultPopup :: String -> X ()
myDefaultPopup s = do
  myPopup 400 0.5 s
  myAlert s
myDefaultPopupWOAlert :: String -> X ()
myDefaultPopupWOAlert = myPopup 400 0.5

popupCurDesktop :: X()
popupCurDesktop = (W.currentTag <$> gets windowset) >>= myDefaultPopupWOAlert . ("ws: " ++)

-- popupCmdOut :: String -> [String] -> X()
-- popupCmdOut cmd args = do
--   cmdOut <- liftIO (readProcess cmd args [])
--   myPopup 400 0.5 cmdOut

-- popupConfig :: Prime
-- popupConfig = do
--   handleEventHook =+ serverModeEventHookF "XMONAD_POPUP" popup
--   apply $ withUrgencyHook $ MyUrgencyHook popup

myAlert :: String -> X ()
myAlert a = let
  rstrip = reverse . dropWhile isSpace . reverse
  in spawn $ "echo '" ++ rstrip a ++ "' > /home/mhuber/.xmonad/notification.pipe"
