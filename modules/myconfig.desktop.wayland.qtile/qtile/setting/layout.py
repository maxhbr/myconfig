from libqtile import layout
from libqtile.config import Match


def init_layouts():
    layout_theme = {"border_width": 2,
                    "margin": 8,
                    "border_focus": "e1acff",
                    "border_normal": "1D2330"
                    }

    layouts = [
        # layout.MonadWide(**layout_theme),
        # layout.Bsp(**layout_theme),
        #layout.Stack(stacks=2, **layout_theme),
        # layout.Columns(**layout_theme),
        # layout.RatioTile(**layout_theme),
        #layout.Tile(shift_windows=True, **layout_theme),
        # layout.VerticalTile(**layout_theme),
        # layout.Matrix(**layout_theme),
        # layout.Zoomy(**layout_theme),
        layout.MonadTall(**layout_theme),
        layout.Max(**layout_theme),
        layout.Stack(num_stacks=2),
        layout.RatioTile(**layout_theme),
        # layout.TreeTab(
        #      font = "Ubuntu",
        #      fontsize = 10,
        #      sections = ["FIRST", "SECOND", "THIRD", "FOURTH"],
        #      section_fontsize = 10,
        #      border_width = 2,
        #      bg_color = "1c1f24",
        #      active_bg = "c678dd",
        #      active_fg = "000000",
        #      inactive_bg = "a9a1e1",
        #      inactive_fg = "1c1f24",
        #      padding_left = 0,
        #      padding_x = 0,
        #      padding_y = 5,
        #      section_top = 10,
        #      section_bottom = 20,
        #      level_shift = 8,
        #      vspace = 3,
        #      panel_width = 200
        #      ),
        layout.Floating(**layout_theme)
    ]
    floating_layout = layout.Floating(float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        # default_float_rules include: utility, notification, toolbar, splash, dialog,
        # file_progress, confirm, download and error.
        *layout.Floating.default_float_rules,
        Match(title='Confirmation'),      # tastyworks exit box
        Match(title='Qalculate!'),        # qalculate-gtk
        Match(wm_class='kdenlive'),       # kdenlive
        Match(wm_class='pinentry-gtk-2'),  # GPG key password entry
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
        Match(wm_class="pavucontrol"), Match(title="Volume Control"),
    ])

    return layouts, floating_layout
