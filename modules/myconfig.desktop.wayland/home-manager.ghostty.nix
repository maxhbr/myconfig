# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Ghostty as the eDP-1 backdrop terminal. Its quick terminal is a
# wlr-layer-shell surface placed in the *bottom* layer, i.e. behind all
# normal windows but above the wallpaper (swaybg). It hosts the shared
# `host-tmux-session`, so the tmux dashboard is always visible behind the
# windows of the focused workspace.
#
# `place-within-backdrop` (niri layer-rule) is NOT used on purpose: it
# only applies to background-layer surfaces that ignore exclusive zones
# and disables input entirely. The bottom layer achieves "terminal behind
# windows" while keeping keyboard focus when clicked
# (quick-terminal-keyboard-interactivity = on-demand).
# https://ghostty.org/docs/config/reference#gtk-quick-terminal-layer
{
  lib,
  pkgs,
  config,
  ...
}:

let
  # The quick terminal spawns the global `command` on every open
  # (`initial-command` only applies to the first surface ever created).
  # Ghostty here is dedicated to the backdrop role, so every surface hosts
  # the shared tmux session (the foot terminal remains the primary one).
  backdropCommand = "host-tmux-session";
in
{
  config = lib.mkIf config.programs.ghostty.enable {
    programs.ghostty = {
      settings = {
        theme = "Catppuccin Mocha";
        background-opacity = 0.85;
        command = backdropCommand;
        # quick terminal as eDP-1 backdrop:
        gtk-quick-terminal-layer = "bottom";
        gtk-quick-terminal-namespace = "ghostty-quick-terminal";
        quick-terminal-position = "top";
        quick-terminal-size = "100%,100%";
        quick-terminal-autohide = false;
        quick-terminal-keyboard-interactivity = "on-demand";
        # ghostty 1.3.1 has no external toggle IPC yet (the
        # `+toggle-quick-terminal` D-Bus action lands upstream in 1.4.0),
        # so this bind only works while a ghostty surface is focused.
        keybind = [ "ctrl+shift+f12=toggle_quick_terminal" ];
      };
    };
    myconfig.desktop.wayland.launcherCommands = [ "ghostty" ];
  };
}
