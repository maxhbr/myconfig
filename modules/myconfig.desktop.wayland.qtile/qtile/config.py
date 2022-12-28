# -*- coding: utf-8 -*-
import os
import re
import socket
import subprocess

from libqtile import qtile, hook
from typing import List  # noqa: F401

from setting.groups import init_groups
from setting.keys import initial_keys
from setting.layout import init_layouts
from setting.screen import init_screens

mod = "mod4"
if qtile.core.name == "x11":
    myTerm = "urxvt"
elif qtile.core.name == "wayland":
    myTerm = "foot"
myLauncher = "wofi --show run"

###############################################################################
###############################################################################
###############################################################################

def set_keymap_from_layout(qtile):
    try:
        qtile.core.cmd_set_keymap(layout=os.environ["XKB_DEFAULT_LAYOUT"], options="", variant=os.environ["XKB_DEFAULT_VARIANT"])
    except:
        print("failed to set layout: with qtile.core.cmd_set_keymap") 
    try:
        qtile.cmd_set_keymap(layout=os.environ["XKB_DEFAULT_LAYOUT"], options="", variant=os.environ["XKB_DEFAULT_VARIANT"])
    except:
        print("failed to set layout: with qtile.cmd_set_keymap") 
if __name__ in ["config", "__main__"]:
    set_keymap_from_layout(qtile)

###############################################################################
###############################################################################
###############################################################################

keys, mouse = initial_keys(mod, myTerm, myLauncher)
groups = init_groups(mod, keys)

# see https://docs.qtile.org/en/stable/manual/config/groups.html
# allow mod3+1 through mod3+0 to bind to groups; if you bind your groups
# by hand in your config, you don't need to do this.
from libqtile.dgroups import simple_key_binder
dgroups_key_binder = simple_key_binder(mod)

layouts, floating_layout = init_layouts()

prompt = "{0}@{1}: ".format(os.environ["USER"], socket.gethostname())

screens = init_screens()

# dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False

auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

@hook.subscribe.startup_once
def start_once():
    home = os.path.expanduser('~')
    # subprocess.call([home + '/.config/qtile/autostart.sh'])

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
