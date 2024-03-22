# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

from libqtile import bar, hook, layout, qtile, widget
from libqtile.config import Click, Drag, DropDown, Group, Key, Match, ScratchPad, Screen
from libqtile.lazy import lazy
import json
import os
import subprocess

process = subprocess.Popen('~/.config/change-wallpaper.sh', shell=True, stdout=subprocess.PIPE)
process.wait()

colors = os.path.expanduser('~/.cache/wal/colors.json')
colordict = json.load(open(colors))
wal_foreground = colordict['special']['foreground']
wal_background = colordict['special']['background']
wal_cursor = colordict['special']['cursor']
wal_colors = [colordict['colors']['color' + str(i)] for i in range(16)]

follow_mouse_focus = True
bring_front_click = False
floats_kept_above = True
cursor_warp = True
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True
auto_minimize = True
wl_input_rules = None
wmname = "qtile"

terminal = "kitty"
file_manager = "nemo"
web_browser = "firefox"
sysmon = terminal + " htop"

keys = []

layouts = [
    layout.MonadTall(
        border_width = 2,
        margin = 8,
        border_focus = wal_colors[3],
        border_normal = wal_background,
        ),
    # layout.MonadWide(),
    layout.Max(),
]

floating_layout_theme = {"border_width": 2,
                "border_focus": wal_colors[7],
                "border_normal": wal_background}

groups = [
    Group("1"),
    Group("2"),
    Group("3"),
    Group("4"),
    Group("5"),
    Group("6"),
    Group("7"),
    Group("8"),
    Group("9", matches=[Match(wm_class = "discord", title = "Discord Updater")]),
    Group("0"),
        ]

def go_to_group(qtile,group_name):
    for s in qtile.screens:
        if s.group.name == group_name:
            qtile.cmd_to_screen(qtile.screens.index(s))
            return
    qtile.groups_map[group_name].toscreen()

def switch_screens(qtile):
    i = qtile.screens.index(qtile.current_screen)
    group = qtile.screens[i - 1].group
    qtile.current_screen.set_group(group)

dgroups_key_binder = None

dgroups_app_rules = []  # type: list

groups.append(ScratchPad("scratchpad", [
        DropDown("quake_term", terminal, height=0.4, width=1, x=0, y=0, opacity=0.9, on_focus_lost_hide=False),
]))

mod = "mod4"

keys.extend([
    Key([mod], "j", lazy.layout.down(), desc="Move focus to next window"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus to previous window"),
])

keys.extend([
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
])

keys.extend([
    Key([mod, "control"], "j", lazy.next_screen(), desc="Move focus to next monitor"),
    Key([mod, "control"], "k", lazy.prev_screen(), desc="Move focus to previous monitor"),
])

keys.append(Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"))

keys.extend([
    Key([mod], "h", lazy.layout.shrink_main(), desc="Shrink window to the left"),
    Key([mod], "l", lazy.layout.grow_main(), desc="Shrink window to the right"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    Key([mod], "s", lazy.function(switch_screens), desc="Switch the groups on the two screens"),
])

keys.extend([
    Key([mod], "f", lazy.window.toggle_fullscreen(), desc="Toggle fullscreen on the focused window",),
    Key([mod], "t", lazy.window.toggle_floating(), desc="Toggle floating on the focused window"),
    Key([mod], "m", lazy.window.toggle_minimize(), desc="Toggle Minimize Window"),
    Key([mod, "shift"], "m", lazy.window.unminimize_all(), desc="Unminimize all windows in group"),
])

keys.extend([
    Key([mod, "shift"], "q", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, "shift"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "shift"], "e", lazy.shutdown(), desc="Shutdown Qtile"),
])

keys.extend([
    Key([mod], "r", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),
    Key([mod], "d", lazy.spawn("rofi -dpi -show drun -theme /home/reiti/.config/awesome/configuration/rofi/appmenu/rofi.rasi"),
        desc="Spawn a command using a prompt widget"),

    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    Key([mod, "shift"], "Return", lazy.spawn(file_manager), desc="Launch File Manager"),
    Key([mod], "w", lazy.spawn(web_browser), desc="Launch Web Browser"),
    Key([mod], "e", lazy.spawn("emacsclient -c"), desc="Launch Emacs"),

    Key(["control", "mod1"], "Delete", lazy.spawn(sysmon), desc="Launch System Monitor"),
    Key([mod], "Print", lazy.spawn("flameshot gui"), desc="Screenshot"),
])

for i in groups:
    if len(i.name) == 1:
        keys.extend(
        [
                # mod1 + group number = switch to group
                Key(
                [mod],
                i.name,
                #lazy.group[i.name].toscreen(),      # the default behaviour
                lazy.function(go_to_group, i.name),  # my alternative function
                desc="Switch to group {}".format(i.name),
                ),
                # mod1 + shift + group number = switch to & move focused window to group
                Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name, switch_group=False),
                desc="Switch to & move focused window to group {}".format(i.name),
                ),
        ])

keys.extend([
        Key([mod, "control"], 'Return', lazy.group['scratchpad'].dropdown_toggle('quake_term')),
        ])

keys.extend([
    Key([mod], "XF86AudioRaiseVolume", lazy.spawn("amixer -D pulse sset Master 5%+"), desc="Raise Volume by 5%"),
    Key([mod], "XF86AudioLowerVolume", lazy.spawn("amixer -D pulse sset Master 5%-"), desc="Lower Volume by 5%"),
    Key([mod], "XF86AudioMute", lazy.spawn("amixer -D pulse set Master 1+ toggle"), desc="Toggle Mute Audio"),
    Key([mod], "XF86AudioNext", lazy.spawn("mpc next"), desc="Music Next"),
    Key([mod], "XF86AudioPrev", lazy.spawn("mpc prev"), desc="Music Previous"),
    Key([mod], "XF86AudioPlay", lazy.spawn("mpc toggle"), desc="Play/Pause Music"),
    Key([mod], "XF86AudioMicMute", lazy.spawn("amixer set Capture toggle"), desc="Toggle Mute Microphone"),
    Key([mod], "XF86MonBrightnessUp", lazy.spawn("light -A 10"), desc="Increase Brightness by 10%"),
    Key([mod], "XF86MonBrightnessDown", lazy.spawn("light -U 10"), desc="Increase Brightness by 10%"),
])

mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

floating_layout = layout.Floating(**floating_layout_theme,
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ],
)

widget_defaults = dict(
    font="Ubuntu Nerd Font",
    fontsize=15,
    padding=3,
)
extension_defaults = widget_defaults.copy()

def init_widget_list(with_systray):
        widget_list = [
                        widget.GroupBox(
                                highlight_method = 'line',
                                disable_drag = True,
                                font = "K2D ExtraBold",
                                hide_unused = False,
                                highlight_color = ['151515C0','303030C0'], # background gradient
                                inactive = '505050', # font color
                                this_current_screen_border = wal_colors[1],
                                this_screen_border = wal_colors[1],
                                other_current_screen_border = wal_colors[7],
                                other_screen_border = wal_colors[7],
                                urgent_alert_method = 'line',
                                urgent_border = 'FF0000',
                                urgent_text = '000000',
                                use_mouse_wheel = False,
                        ),
                        widget.Prompt(),
                        widget.Chord(
                                chords_colors={
                                        "launch": ("#ff0000", "#ffffff"),
                                },
                                name_transform=lambda name: name.upper(),
                        ),
                        widget.Spacer(),
                        widget.TaskList(
                                highlight_method = 'border',
                                border = wal_colors[3],
                                borderwidth = 2,
                                unfocused_border = None,
                                max_title_width = 250,
                                markup_minimized = "<i>({})</i>",
                                markup_maximized = "<b>{}</b>",
                                txt_floating = "🗗 ",
                                txt_maximized = "🗖 ",
                                txt_minimized = "🗕 ",
                                fontsize = 14,
                                foreground = 'ffffff', # font color
                                margin_y = 4,
                                width = bar.CALCULATED,
                        ),
                        widget.Spacer(),
                        widget.WidgetBox(
                                close_button_location = 'right',
                                start_opened = False,
                                text_closed = '󰝡',
                                text_open = '󰝠',
                                fontsize = 20,
                                widgets=[widget.Systray()]
                        ),
                        widget.Clock(
                                format="%H:%M, %A %-d. %B %Y",
                                update_interval = 1.0,
                                padding = 4,
                        ),
                        widget.CurrentLayoutIcon(
                                scale = 0.5,
                                padding = 5,
                        ),
                ]
        if not with_systray:
                widget_list.pop(-3) # systray is third to last widget
        return widget_list

my_bars = [bar.Bar(
            init_widget_list(with_systray),
            size = 40,
            background = '#00000080', # transparent background
            opacity = 1, # but no transparency of widgets
            border_width = 0,
            reserve = True,
        ) for with_systray in [True, False]]

screens = [
    Screen(bottom=my_bars[0]),
    Screen(bottom=my_bars[1]),
]

@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser('~/.config/qtile/autostart.sh')
    subprocess.Popen([home])
