from __future__ import annotations
from typing import Callable, Any

import os
import pwd
import time
import logging

logger = logging.getLogger(__name__)
from newm.layout import Layout
from newm.helper import BacklightManager, WobRunner, PaCtl

from pywm import (
    PYWM_MOD_LOGO,
    PYWM_MOD_ALT,
    PYWM_TRANSFORM_90,
    PYWM_TRANSFORM_180,
    PYWM_TRANSFORM_270,
    PYWM_TRANSFORM_FLIPPED,
    PYWM_TRANSFORM_FLIPPED_90,
    PYWM_TRANSFORM_FLIPPED_180,
    PYWM_TRANSFORM_FLIPPED_270,
)

def on_startup():
    os.system("dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=wlroots")

background = {
    'path': os.path.dirname(os.path.realpath(__file__)) + '/resources/wallpaper.jpg',
    'anim': True
}

outputs = [
    { 'name': 'eDP-1' }
]


def rules(view):
    if view.app_id == "catapult":
        return { 'float': True, 'float_pos': (0.5, 0.25) }
    if view.app_id == "pavucontrol":
        return { 'float': True, 'float_size': (340, 600), 'float_pos': (0.15, 0.4) }
    if view.title is not None and view.title.strip() == "Firefox — Sharing Indicator":
        return { 'float': True, 'float_size': (100, 40), 'float_pos': (0.5, 0.1) }
    if view.app_id == "Alacritty":
        return { 'blur': { 'radius': 5, 'passes': 3}}
    if view.app_id == "waybar":
        return { 'blur': { 'radius': 5, 'passes': 3}}
    return None

view = {
    'padding': 8,
    'fullscreen_padding': 0,
    'send_fullscreen': False,
    'accept_fullscreen': False,

    'rules': rules,
    'floating_min_size': False,

    'debug_scaling': True,
    'border_ws_switch': 100,
}


wob_runner = WobRunner("wob -a bottom -M 100")
backlight_manager = BacklightManager(anim_time=1., bar_display=wob_runner)
kbdlight_manager = BacklightManager(args="--device='*::kbd_backlight'", anim_time=1., bar_display=wob_runner)
def synchronous_update() -> None:
    backlight_manager.update()
    kbdlight_manager.update()

pactl = PaCtl(0, wob_runner)

def key_bindings(layout: Layout) -> list[tuple[str, Callable[[], Any]]]:
    return [
        ("L-n", lambda: layout.move(-1, 0)),
        ("L-r", lambda: layout.move(0, 1)),
        ("L-t", lambda: layout.move(0, -1)),
        ("L-d", lambda: layout.move(1, 0)),
        ("L-b", lambda: layout.basic_scale(1)),
        ("L-s", lambda: layout.basic_scale(-1)),
        ("L-g", lambda: layout.move_in_stack(1)),

        ("L-N", lambda: layout.move_focused_view(-1, 0)),
        ("L-R", lambda: layout.move_focused_view(0, 1)),
        ("L-T", lambda: layout.move_focused_view(0, -1)),
        ("L-D", lambda: layout.move_focused_view(1, 0)),

        ("L-C-n", lambda: layout.resize_focused_view(-1, 0)),
        ("L-C-r", lambda: layout.resize_focused_view(0, 1)),
        ("L-C-t", lambda: layout.resize_focused_view(0, -1)),
        ("L-C-d", lambda: layout.resize_focused_view(1, 0)),

        ("L-Return", lambda: os.system("tfoot &")),
        ("L-p", lambda: os.system("wofi --show run &")),
        ("L-r", lambda: os.system("rofi -show run &")),
        ("L-C", lambda: layout.close_focused_view()),

        ("L-Y", lambda: layout.ensure_locked(dim=True)),
        ("L-Q", lambda: layout.terminate()),
        ("L-C", lambda: layout.update_config()),

        ("L-f", lambda: layout.toggle_fullscreen()),

        ("L-", lambda: layout.toggle_overview()),

        ("XF86MonBrightnessUp", lambda: backlight_manager.set(backlight_manager.get() + 0.1)),
        ("XF86MonBrightnessDown", lambda: backlight_manager.set(backlight_manager.get() - 0.1)),
        ("XF86KbdBrightnessUp", lambda: kbdlight_manager.set(kbdlight_manager.get() + 0.1)),
        ("XF86KbdBrightnessDown", lambda: kbdlight_manager.set(kbdlight_manager.get() - 0.1)),
        ("XF86AudioRaiseVolume", lambda: pactl.volume_adj(5)),
        ("XF86AudioLowerVolume", lambda: pactl.volume_adj(-5)),
        ("XF86AudioMute", lambda: pactl.mute()),
    ]

panels = {
    # 'lock': {
    #     'cmd': 'myphyslock',
    # },
    'lock': {
        'cmd': 'alacritty -e newm-panel-basic lock',
        'w': 0.7,
        'h': 0.6,
        'corner_radius': 50,
    },
    'launcher': {
        'cmd': 'alacritty -e newm-panel-basic launcher'
    },
    'bar': {
        'cmd': 'waybar'
    },
}

grid = {
    'throw_ps': [2, 10]
}

energy = {
    'idle_times': [60, 180],
    'idle_callback': backlight_manager.callback
}

focus = {
    'color': '#92f0f5d1',
    'enabled': True
}
