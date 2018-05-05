-- Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
module XMonad.MyConfig.MyManageHookLayer
    ( applyMyManageHook
    ) where

import           XMonad

import           XMonad.Hooks.ManageHelpers ( doCenterFloat )

applyMyManageHook c = c { manageHook = (manageHook c) <+> myManageHook }

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
  baseHooks = foldMap (\(a,cs) -> map (\c -> className =? c --> a) cs)
                            [ (doCenterFloat, ["Xmessage"
                                              ,"qemu","qemu-system-x86_64"
                                              ,"feh"
                                              ,"Zenity"
                                              ,"pinentry","Pinentry"
                                              ,"pavucontrol","Pavucontrol"
                                              ,"zoom"])
                            , (doFloat, ["MPlayer"
                                        ,"Onboard"])
                            , (doShift "web", ["Firefox"
                                              ,"Chromium","chromium-browser"])
                            , (doShift "10", ["franz","Franz"])
                            , (doShift "vbox", ["Virtualbox","VirtualBox"])
                            , (doShift "media", ["Steam"])
                            , (doIgnore, ["desktop_window"
                                         ,"kdesktop"]) ]
  -- see:
  -- - https://www.reddit.com/r/xmonad/comments/78uq0p/how_do_you_deal_with_intellij_idea_completion/?st=jgdc0si0&sh=7d79c956
  -- - https://youtrack.jetbrains.com/issue/IDEA-112015#comment=27-2498787
  ideaPopupHook = [ appName =? "sun-awt-X11-XWindowPeer" <&&> className =? "jetbrains-idea" --> doIgnore ]
  in composeAll (baseHooks ++ ideaPopupHook)
