from libqtile.config import Group, ScratchPad, DropDown, Key, Match
from libqtile.config import Key
from libqtile.lazy import lazy

def init_groups(mod, keys):
    groups = [Group("1", layout='monadtall'),
            Group("2", layout='monadtall'),
            Group("3", layout='monadtall'),
            Group("4", layout='monadtall'),
            Group("5", layout='monadtall'),
            Group("6", layout='monadtall'),
            Group("7", layout='monadtall'),
            Group("8", layout='monadtall', matches=[Match(wm_class=["Firefox"])]),
            Group("9", layout='monadtall'),
            Group("0", layout='monadtall'),
            ScratchPad("scratchpad", [
                DropDown("term", "tfoot", opacity=0.8),
                DropDown("scratch", "foot-scratch",
                        x=0.05, y=0.4, width=0.9, height=0.6, opacity=0.9,
                        on_focus_lost_hide=True),
                ])
            ]

    keys.extend([
        Key([mod], 'F11', lazy.group['scratchpad'].dropdown_toggle('term')),
        Key([mod], 'minus', lazy.group['scratchpad'].dropdown_toggle('scratch')) 
    ])

    return groups
