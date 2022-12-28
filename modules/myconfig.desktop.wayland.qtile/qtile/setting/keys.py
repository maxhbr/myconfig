from libqtile.command import lazy
from libqtile.config import Click, Key, Drag, KeyChord
from libqtile.lazy import lazy


def window_to_prev_group(qtile):
    if qtile.currentWindow is not None:
        i = qtile.groups.index(qtile.currentGroup)
        qtile.currentWindow.togroup(qtile.groups[i - 1].name)


def window_to_next_group(qtile):
    if qtile.currentWindow is not None:
        i = qtile.groups.index(qtile.currentGroup)
        qtile.currentWindow.togroup(qtile.groups[i + 1].name)


def window_to_previous_screen(qtile):
    i = qtile.screens.index(qtile.current_screen)
    if i != 0:
        group = qtile.screens[i - 1].group.name
        qtile.current_window.togroup(group)


def window_to_next_screen(qtile):
    i = qtile.screens.index(qtile.current_screen)
    if i + 1 != len(qtile.screens):
        group = qtile.screens[i + 1].group.name
        qtile.current_window.togroup(group)


def switch_screens(qtile):
    i = qtile.screens.index(qtile.current_screen)
    group = qtile.screens[i - 1].group
    qtile.current_screen.set_group(group)


def initial_keys(mod, myTerm, myLauncher):
    keys = [
        # The essentials
        Key([mod], "Return",
            lazy.spawn(myTerm),
            desc='Launches My Terminal'
            ),
        Key([mod, "shift"], "Return",
            lazy.spawn(myTerm),
            desc='Launches My Terminal'
            ),
        Key([mod], "p",
            lazy.spawn(myLauncher),
            desc='launcher'
            ),
        Key([mod], "space",
            lazy.next_layout(),
            desc='Toggle through layouts'
            ),
        Key([mod, "shift"], "c",
            lazy.window.kill(),
            desc='Kill active window'
            ),
        Key([mod, "shift"], "r",
            lazy.restart(),
            desc='Restart Qtile'
            ),
        Key([mod, "shift"], "q",
            lazy.shutdown(),
            desc='Shutdown Qtile'
            ),
        Key(["control", "shift"], "e",
            lazy.spawn("emacsclient -c -a emacs"),
            desc='Doom Emacs'
            ),
        # Switch focus to specific monitor (out of three)
        Key([mod], "w",
            lazy.to_screen(0),
            desc='Keyboard focus to monitor 1'
            ),
        Key([mod], "e",
            lazy.to_screen(1),
            desc='Keyboard focus to monitor 2'
            ),
        Key([mod], "r",
            lazy.to_screen(2),
            desc='Keyboard focus to monitor 3'
            ),
        # Switch focus of monitors
        Key([mod], "period",
            lazy.next_screen(),
            desc='Move focus to next monitor'
            ),
        Key([mod], "comma",
            lazy.prev_screen(),
            desc='Move focus to prev monitor'
            ),
        # Treetab controls
        Key([mod, "shift"], "h",
            lazy.layout.move_left(),
            desc='Move up a section in treetab'
            ),
        Key([mod, "shift"], "l",
            lazy.layout.move_right(),
            desc='Move down a section in treetab'
            ),
        # Window controls
        Key([mod], "j",
            lazy.layout.down(),
            desc='Move focus down in current stack pane'
            ),
        Key([mod], "k",
            lazy.layout.up(),
            desc='Move focus up in current stack pane'
            ),
        Key([mod, "shift"], "j",
            lazy.layout.shuffle_down(),
            lazy.layout.section_down(),
            desc='Move windows down in current stack'
            ),
        Key([mod, "shift"], "k",
            lazy.layout.shuffle_up(),
            lazy.layout.section_up(),
            desc='Move windows up in current stack'
            ),
        Key([mod], "h",
            lazy.layout.shrink(),
            lazy.layout.decrease_nmaster(),
            desc='Shrink window (MonadTall), decrease number in master pane (Tile)'
            ),
        Key([mod], "l",
            lazy.layout.grow(),
            lazy.layout.increase_nmaster(),
            desc='Expand window (MonadTall), increase number in master pane (Tile)'
            ),
        Key([mod], "n",
            lazy.layout.normalize(),
            desc='normalize window size ratios'
            ),
        Key([mod], "m",
            lazy.layout.maximize(),
            desc='toggle window between minimum and maximum sizes'
            ),
        Key([mod, "shift"], "f",
            lazy.window.toggle_floating(),
            desc='toggle floating'
            ),
        Key([mod], "f",
            lazy.window.toggle_fullscreen(),
            desc='toggle fullscreen'
            ),
        # Stack controls
        Key([mod, "shift"], "space",
            lazy.layout.rotate(),
            lazy.layout.flip(),
            desc='Switch which side main pane occupies (XmonadTall)'
            ),
        Key([mod], "Tab",
            lazy.layout.next(),
            desc='Switch window focus to other pane(s) of stack'
            ),
        Key([mod, "shift"], "Tab",
            lazy.layout.toggle_split(),
            desc='Toggle between split and unsplit sides of stack'
            ),
    ]
    mouse = [
        Drag([mod], "Button1", lazy.window.set_position_floating(),
             start=lazy.window.get_position()),
        Drag([mod], "Button3", lazy.window.set_size_floating(),
             start=lazy.window.get_size()),
        Click([mod], "Button2", lazy.window.bring_to_front())
    ]
    return keys, mouse
