-- Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
module XMonad.MyConfig.MyManageHookLayer
  ( applyMyManageHook
  ) where

import           XMonad

import           Data.List                    (isSuffixOf)
import qualified XMonad.StackSet              as W

import           XMonad.Hooks.DynamicProperty (dynamicTitle)
import           XMonad.Hooks.ManageHelpers   (composeOne, doCenterFloat,
                                               doFullFloat, doRectFloat,
                                               isDialog, isFullscreen,
                                               transience, (-?>))
import           XMonad.StackSet              (RationalRect (..))

applyMyManageHook c =
  let manageHook' = myManageHook <+> manageHook c
   in c
        { manageHook = manageHook'
    -- , handleEventHook = dynamicTitle manageHook' <> handleEventHook c
        }

------------------------------------------------------------------------
-- Window rules:
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook =
  let hookForDialogs = isDialog -?> doCenterFloat
      fullscreenHook = isFullscreen -?> doFullFloat
      gtkFileChooserHook =
        stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" -?>
        doCenterFloat
      hooksByClassName =
        foldMap
          (\(a, cs) -> map (\c -> className =? c -?> a) cs)
          [ ( doCenterFloat
            , ["Xmessage", "Zenity", "pinentry", "Pinentry", "feh"])
          , ( doRectFloat (RationalRect 0.1 0.05 0.3 0.9)
            , ["pavucontrol", "Pavucontrol"])
          , (doFloat, ["MPlayer", "Onboard"])
          , (doShift "9mail", [".evolution-wrapped_", "evolution"])
          , (doShift "9", ["franz", "Franz", "Telegram"])
          , (doShift "9", ["slack", "Slack"])
          , ( doShift "media"
            , ["Steam", "google-chrome", "Google-chrome", "Xephyr"])
          , ( doShift "13"
            , [ "Wfica"
              , "Wfica_Seamless"
              , "vncviewer"
              , "Vncviewer"
              , "org.remmina.Remmina"
              ])
          , (doIgnore, ["desktop_window", "kdesktop", "xmessage", "Xmessage"])
          ]
    -- see:
    -- - https://www.reddit.com/r/xmonad/comments/78uq0p/how_do_you_deal_with_intellij_idea_completion/?st=jgdc0si0&sh=7d79c956
    -- - https://youtrack.jetbrains.com/issue/IDEA-112015#comment=27-2498787
      ideaPopupHook =
        appName =? "sun-awt-X11-XWindowPeer" <&&> className =? "jetbrains-idea" -?>
        doIgnore
    -- see: https://www.peterstuart.org/posts/2021-09-06-xmonad-zoom/
      manageZoomHooks =
        composeAll $
        [ (className =? zoomClassName) <&&> shouldFloat <$>
          title --> doFloat <+> doF W.focusDown
        , (className =? zoomClassName) <&&> shouldSink <$>
          title --> (ask >>= doF . W.sink) <+> doF W.swapDown
        ]
        where
          zoomClassName = "zoom"
          tilePreds =
            [ (==) "Zoom - Free Account" -- main window
            , (==) "Zoom - Licensed Account" -- main window
            , (==) "Zoom" -- meeting window on creation / second window
            , ("Zoom Meeting" `isSuffixOf`) -- meeting window shortly after creation
            ]
          shouldSink title = any (\f -> f title) tilePreds
          shouldFloat = not . shouldSink
   in composeOne
        (ideaPopupHook :
         (transience :
          (hooksByClassName ++
           [hookForDialogs, fullscreenHook, gtkFileChooserHook]))) <>
      manageZoomHooks
