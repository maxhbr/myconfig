import os

from libqtile.config import Screen
from libqtile import widget, bar, qtile

# prompt = "{0}@{1}: ".format(os.environ["USER"], socket.gethostname())

colors = [["#282c34", "#282c34"],
          ["#1c1f24", "#1c1f24"],
          ["#dfdfdf", "#dfdfdf"],
          ["#ff6c6b", "#ff6c6b"],
          ["#98be65", "#98be65"],
          ["#da8548", "#da8548"],
          ["#51afef", "#51afef"],
          ["#c678dd", "#c678dd"],
          ["#46d9ff", "#46d9ff"],
          ["#a9a1e1", "#a9a1e1"]]

##### DEFAULT WIDGET SETTINGS #####
widget_defaults = dict(
    font="Ubuntu Bold",
    fontsize=10,
    padding=2,
    background=colors[2]
)
extension_defaults = widget_defaults.copy()


def init_widgets_list(withSystray):
    sep = widget.Sep(
        linewidth=0,
        padding=6,
        foreground=colors[2],
        background=colors[0]
    )
    bar = widget.TextBox(
        text='|',
        font="Ubuntu Mono",
        background=colors[0],
        foreground='474747',
        padding=2,
        fontsize=14
    )
    widgets_list = [
        sep,
        sep,
        widget.GroupBox(
            font="Ubuntu Bold",
            fontsize=9,
            margin_y=3,
            margin_x=0,
            padding_y=5,
            padding_x=3,
            borderwidth=3,
            active=colors[2],
            inactive=colors[7],
            rounded=False,
            highlight_color=colors[1],
            highlight_method="line",
            this_current_screen_border=colors[6],
            this_screen_border=colors[4],
            other_current_screen_border=colors[6],
            other_screen_border=colors[4],
            foreground=colors[2],
            background=colors[0]
        ),
        bar,
        widget.CurrentLayoutIcon(
            custom_icon_paths=[os.path.expanduser("~/.config/qtile/icons")],
            foreground=colors[2],
            background=colors[0],
            padding=0,
            scale=0.7
        ),
        widget.CurrentScreen(
            background=colors[0],
        ),
        bar,
        widget.WindowName(
            foreground=colors[6],
            background=colors[0],
            padding=0
        ),
        sep,
        widget.Battery(
            foreground=colors[6],
            background=colors[0],
        ),
        bar,
        widget.Clock(
            foreground=colors[6],
            background=colors[0],
            format="%A, %B %d - %H:%M "
        ),
    ]
    if withSystray:
        widgets_list.append(
            widget.Systray(
                background=colors[0],
                padding=5
            )
        )
    return widgets_list


def init_screens():
    screens = []
    for output in qtile.core.outputs:
        wlr_output = output.wlr_output
        screens.append(
            Screen(top=bar.Bar(widgets=init_widgets_list(screens == []), opacity=1.0, size=20))
        )
    return screens