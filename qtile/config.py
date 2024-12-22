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

from libqtile import bar, hook, qtile, layout, widget
from libqtile.config import Click, Drag, DropDown, Group, Key, Match, ScratchPad, Screen
from libqtile.lazy import lazy
import json
import os
import subprocess
import colorsys

hostname = subprocess.Popen("hostname", stdout=subprocess.PIPE ).communicate()[0]
hostname = hostname.strip()  # remove trailing newline
hostname = hostname.decode("utf-8")  # decode from type 'byte' to type 'str'

if hostname == "One":
    config_in_use = "desktop"
    desktop = True
    laptop = False
else:
    config_in_use = "laptop"
    desktop = False
    laptop = True

process = subprocess.Popen('~/.config/change-wallpaper.sh', shell=True, stdout=subprocess.PIPE)
process.wait()

colors = os.path.expanduser('~/.cache/wal/colors.json')
colordict = json.load(open(colors))
wal_foreground = colordict['special']['foreground']
wal_background = colordict['special']['background']
wal_cursor = colordict['special']['cursor']
wal_colors = [colordict['colors']['color' + str(i)] for i in range(16)]

def string_to_rgb(col):
    col = col.strip('#')
    if len(col) not in {6,8}:
        raise ValueError("Not a valid color string.")
    red = int(col[0:2], 16)/255
    green = int(col[2:4], 16)/255
    blue = int(col[4:6], 16)/255
    if len(col) == 8:
        alpha = int(col[6:8], 16)/255
        return (red,green,blue,alpha)
    else:
        return (red,green,blue)

def rgb_to_string(red,green,blue, alpha=None):
    col = [red,green,blue,alpha]
    if alpha == None:
        col.remove(None)
    if not all([0<=num<=1 for num in col]):
        raise ValueError("Color values must be between 0 and 1.")
    for i in range(len(col)):
        col[i] = hex(round(255*col[i]))[2:]
        while len(col[i]) < 2:
            col[i] = "0" + col[i]
    return ''.join(col)

def modify_helper(col, value, i):
    if type(col) == str:
        col_rgb = list(string_to_rgb(col))
    if type(col) == list:
        col_rgb = col.copy
    if type(col) == tuple:
        col_rgb = list(col)

    if len(col_rgb) == 4:
        alpha = col_rgb[3]
    else:
        alpha = None

    col_hsl = colorsys.rgb_to_hls(*col_rgb[0:3]) # remove alpha
    current_value = col_hsl[i]
    if type(value) == str:
        value = value.strip('%')
        perc = int(value)/100
        new_value = max(0, min(1, current_value*(1+perc)))
    else:
        new_value = float(value)
    col_hsl = list(col_hsl)
    col_hsl[i] = new_value
    col_rgb = colorsys.hls_to_rgb(*col_hsl)
    col_rgb = list(col_rgb)
    if alpha != None:
        col_rgb.append(alpha)

    if type(col) == str:
        return rgb_to_string(*col_rgb)
    if type(col) == list:
        return list(col_rgb)
    if type(col) == tuple:
        return tuple(col_rgb)


def modify_lightness(col,lightness):
    return modify_helper(col,lightness,1)
def modify_saturation(col,saturation):
    return modify_helper(col,saturation,2)

wal_colors_without_background = [col for col in wal_colors if col != wal_background]
wal_hls_without_background = [colorsys.rgb_to_hls(*string_to_rgb(col))
                              for col in wal_colors_without_background]

saturations = [colorsys.rgb_to_hls(*string_to_rgb(col))[2] for col in wal_colors_without_background]
wal_sorted_saturation = [pair[1] for pair in
                         sorted(zip(saturations, wal_colors_without_background),
                                key=lambda pair: -pair[0])]

hue_differences = [abs(col[0] - wal_background_hls[0]) for col in wal_hls_without_background]
wal_sorted_huediff = [pair[1] for pair in
                         sorted(zip(hue_differences, wal_colors_without_background),
                                key=lambda pair: -pair[0])]

wal_background_hls = colorsys.rgb_to_hls(*string_to_rgb(wal_background))
wal_background_lightness = wal_background_hls[1]

# some magic numbers one can tweak
min_lightness = 0.5 * wal_background_lightness
max_lightness = min(2*wal_background_lightness,1)
num_lightness = 5

lightnesses = [min_lightness + i*(max_lightness-min_lightness)/(num_lightness-1)
               for i in range(num_lightness)]
wal_background_versions = [modify_lightness(wal_background,l) for l in lightnesses]

color_primary = modify_lightness(wal_sorted_saturation[0],0.7)
color_primary = modify_saturation(color_primary,"+20%")

color_secondary = modify_lightness(wal_sorted_saturation[1],0.85)
color_secondary = modify_saturation(color_secondary,"+20%")

bar_opacity = "C0"           # two hex digits
color_bar = wal_background + bar_opacity

follow_mouse_focus = True
bring_front_click = "floating_only"
floats_kept_above = True
cursor_warp = False
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True
auto_minimize = True
wl_input_rules = None
wmname = "qtile"

terminal = "kitty"
file_manager = "nemo"
web_browser = "firefox"
emacs = "emacsclient -c -a ''"
sysmon = terminal + " htop"
lockscreen = "xlock -mode \"rain\" -saturation 1 -mousemotion -timeout 10 -password \"Password please.\""
hibernate = ["sh", "-c", lockscreen + "& systemctl hibernate"]
suspend = ["sh", "-c", lockscreen + "& systemctl suspend"]

keys = []

layouts = [
    layout.MonadTall(
        border_width = 2,
        margin = 8,
        border_focus = color_primary,
        ),
    # layout.MonadWide(),
    layout.Max(),
]

floating_layout_theme = {"border_width": 2,
                "border_focus": color_secondary,
                "border_normal": wal_background}

groups = [Group(i) for i in "1234567890"]

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
        DropDown("quake_term", terminal, height=0.4, width=.995, x=.0025, y=0, opacity=0.9, on_focus_lost_hide=False),
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
    Key([mod, "control"], "l", lazy.spawn(lockscreen), desc="Lock screen"),
    Key([mod, "control"], "h", lazy.spawn(hibernate), desc="Hibernate"),
    Key([mod, "control"], "s", lazy.spawn(suspend), desc="Suspend system"),
])

keys.extend([
    Key([mod], "r", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),
    Key([mod], "d", lazy.spawn("rofi -dpi -show drun -theme ~/.config/rofi/rofi.rasi"),
        desc="Spawn a command using a prompt widget"),

    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    Key([mod, "shift"], "Return", lazy.spawn(file_manager), desc="Launch File Manager"),
    Key([mod], "w", lazy.spawn(web_browser), desc="Launch Web Browser"),
    Key([mod], "e", lazy.spawn(emacs), desc="Launch Emacs"),
    Key([mod], "a", lazy.spawn("emacsclient --eval \"(emacs-everywhere)\""), desc="Use Emacs ANYWHERE"),
    Key([mod], "z", lazy.spawn("zotero"), desc="Launch Zotero"),

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
    Key([], "XF86AudioRaiseVolume", lazy.spawn("amixer -D pulse sset Master 5%+"), desc="Raise Volume by 5%"),
    Key([], "XF86AudioLowerVolume", lazy.spawn("amixer -D pulse sset Master 5%-"), desc="Lower Volume by 5%"),
    Key([], "XF86AudioMute", lazy.spawn("amixer -D pulse set Master 1+ toggle"), desc="Toggle Mute Audio"),
    Key([], "XF86AudioNext", lazy.spawn("mpc next"), desc="Music Next"),
    Key([], "XF86AudioPrev", lazy.spawn("mpc prev"), desc="Music Previous"),
    Key([], "XF86AudioPlay", lazy.spawn("mpc toggle"), desc="Play/Pause Music"),
    Key([], "XF86AudioMicMute", lazy.spawn("amixer set Capture toggle"), desc="Toggle Mute Microphone"),
    Key([], "XF86MonBrightnessUp", lazy.spawn("light -A 10"), desc="Increase Brightness by 10%"),
    Key([], "XF86MonBrightnessDown", lazy.spawn("light -U 10"), desc="Decrease Brightness by 10%"),
])

mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

groups[8].matches = [Match(wm_class = "discord", title = "Discord Updater")]
groups[8].spawn = "discord"

groups[9].matches = [Match(wm_class = "spotify")]

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
        Match(title="emacs-everywhere"),
        Match(func=lambda c: ("REAPER" in c.info()['wm_class']) and (not "REAPER v" in c.info()['name'])),
    ],
)

@hook.subscribe.client_managed
def resize_floating(client):
    if "emacs-everywhere" in client.name:
        client.set_size_floating(1200,700)
        client.center()

if laptop:
        @hook.subscribe.client_managed
        def onboard_in_front(client):
                if client.name == "Onboard":
                        client.bring_to_front()

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
                                this_current_screen_border = color_primary,
                                this_screen_border = color_primary,
                                other_current_screen_border = None,
                                other_screen_border = None,
                                urgent_alert_method = 'line',
                                urgent_border = 'FF0000',
                                urgent_text = '000000',
                                use_mouse_wheel = False,
                                padding_x = 8 if laptop else None,
                                fontsize = 18 if laptop else 15,
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
                                border = color_primary,
                                borderwidth = 2,
                                unfocused_border = None,
                                max_title_width = 250,
                                markup_minimized = "<i>({})</i>",
                                markup_maximized = "<b>{}</b>",
                                txt_floating = "🗗 ",
                                txt_maximized = "🗖 ",
                                txt_minimized = "🗕 ",
                                foreground = 'ffffff', # font color
                                margin_y = 6,
                                icon_size = 35,
                                stretch = False,
                        ),
                        widget.Spacer(),
                        widget.WidgetBox(
                                close_button_location = 'right',
                                start_opened = False,
                                text_closed = '󰝡',
                                text_open = '󰝠',
                                fontsize = 20,
                                widgets=[widget.Systray(padding = 8,
                                        background = wal_background_versions[1] + bar_opacity)],
                                padding = 0,
                        ),
                        widget.Clock(
                                format="%H:%M, %A %-d. %B %Y",
                                update_interval = 1.0,
                                padding = 12,
                        ),
                        widget.BatteryIcon(
                                update_interval = 60,
                                theme_path = "~/.config/qtile/icons",
                                scale = 1.05,
                                padding = 0,
                        ),
                        widget.Battery(
                                update_interval = 60,
                                charge_char = "",
                                discharge_char = "",
                                format = "{percent:2.0%} / {hour:d}:{min:02d}h",
                                hide_threshold = None,
                                low_foreground = 'FF0000',
                                low_percentage = 0.11,
                                notify_below = 0.11,
                                notification_timeout = 0,
                                padding = 0,
                        ),
                        widget.CurrentLayoutIcon(
                                scale = 0.5,
                                padding = 12,
                        ),
                ]
        if not with_systray:
                widget_list.pop(-5) # systray is third to last widget
        if config_in_use == "desktop":
                widget_list.pop(-3) # remove battery and battery icon
                widget_list.pop(-2)
                # it's important that we pop things in ascending order
        return widget_list

my_bars = [bar.Bar(
            init_widget_list(with_systray),
            size = 40,
            background = color_bar,
            opacity = 1, # but no transparency of widgets
            border_width = 0,
            reserve = True,
            #margin = [5, 5, -2, 5],
        ) for with_systray in [True, False]]

screens = [
    Screen(top=my_bars[0]),
    Screen(top=my_bars[1]),
]

@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser('~/.config/qtile/autostart.sh')
    subprocess.Popen([home])
