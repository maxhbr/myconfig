-- Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
module XMonad.MyConfig.MyManageHookLayer
    ( applyMyManageHook
    ) where

import           XMonad

import           XMonad.StackSet (RationalRect (..))
import           XMonad.Hooks.ManageHelpers ( composeOne, (-?>)
                                            , doCenterFloat
                                            , doSideFloat, Side (..)
                                            , doRectFloat
                                            , doFullFloat
                                            , transience
                                            , isDialog, isFullscreen)

applyMyManageHook c = c { manageHook = manageHook c <+> myManageHook }

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
myManageHook = let
    hookForDialogs = isDialog -?> doCenterFloat
    fullscreenHook = isFullscreen -?> doFullFloat
    gtkFileChooserHook = stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" -?> doCenterFloat
    hooksByClassName = foldMap (\(a,cs) -> map (\c -> className =? c -?> a) cs)
                               [ (doCenterFloat, ["Xmessage"
                                                 ,"Zenity"
                                                 ,"pinentry","Pinentry"
                                                 ,"feh"])
                               , (doSideFloat NE, ["zoom","zoom-us"])
                               , (doRectFloat (RationalRect 0.1 0.1 0.4 0.4), ["pavucontrol","Pavucontrol"])
                               , (doFloat, ["MPlayer"
                                           ,"Onboard"])
                               , (doShift "10", ["franz","Franz"
                                                ,"rambox"])
                               , (doShift "media", ["Steam", "Wfica", "Wfica_Seamless"])
                               , (doIgnore, ["desktop_window"
                                            ,"kdesktop"
                                            ,"xmessage","Xmessage"]) ]
    -- see:
    -- - https://www.reddit.com/r/xmonad/comments/78uq0p/how_do_you_deal_with_intellij_idea_completion/?st=jgdc0si0&sh=7d79c956
    -- - https://youtrack.jetbrains.com/issue/IDEA-112015#comment=27-2498787
    ideaPopupHook = appName =? "sun-awt-X11-XWindowPeer" <&&> className =? "jetbrains-idea" --> doIgnore
  in composeAll (composeOne ([hookForDialogs, transience, fullscreenHook, gtkFileChooserHook] ++ hooksByClassName) : [ideaPopupHook])
