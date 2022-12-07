-- Copyright 2017-2018 Maximilian Huber <oss@maximilian-huber.de>
-- SPDX-License-Identifier: MIT
-- ~/.xmonad/xmonad.hs
-- needs xorg-xmessage for error messages
--
-- xmonad-extras from cabal
--
-- used software
--  dmenu        to start software
--  dwb          fast browser
--  scrot        screenshot tool
--  imagemagic   screenshot tool
--  slim         screenlock tool
--  unclutter    to hide mouse pointer
--  urxvt        terminal
--  xcalib       invert colors
--  xmobar       bar
--  pass         password manager
import XMonad.MyConfig


------------------------------------------------------------------------
-- Now run xmonad
main = runMyConfig
