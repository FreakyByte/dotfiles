+++
title = "Qtile Config"
author = ["Michael Reitmeir"]
draft = false
weight = 1
+++

## Intro {#intro}


### License {#license}

```python
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
```


### Imports {#imports}

```python
from libqtile import bar, hook, qtile, layout, widget
from libqtile.config import Click, Drag, DropDown, Group, Key, Match, ScratchPad, Screen
from libqtile.lazy import lazy
import json
import os
import subprocess
import colorsys
```


## Different Config on Different Systems {#different-config-on-different-systems}

I use this config on a desktop PC (called "One") and a convertible laptop (called "IdeaPad"). While most things would stay the same between the two, some other things I would like to behave differently, especially since my laptop should also be able to be used using its touchscreen.

To distinguish the two machines, let's look at the hostname:

```python
hostname = subprocess.Popen("hostname", stdout=subprocess.PIPE ).communicate()[0]
hostname = hostname.strip()  # remove trailing newline
hostname = hostname.decode("utf-8")  # decode from type 'byte' to type 'str'
```

Later we use this by consulting the variable `config_in_use` as well as the booleans `desktop` and `laptop` whenever we want to configure something desktop or laptop specific. We default to the laptop config, since I expect this one to have more features.

```python
if hostname == "One":
    config_in_use = "desktop"
    desktop = True
    laptop = False
else:
    config_in_use = "laptop"
    desktop = False
    laptop = True
```


## Wallpaper and Colors {#wallpaper-and-colors}

```python
process = subprocess.Popen('~/.config/change-wallpaper.sh', shell=True, stdout=subprocess.PIPE)
process.wait()
```

Somehow this runs the `change-wallpaper.sh` script twice, don't know why as of now.


### Pywal {#pywal}

I use [pywal](https://github.com/dylanaraps/pywal) to create colorschemes based on the current wallpaper. This lets me use these colors in qtile as well.

```python
colors = os.path.expanduser('~/.cache/wal/colors.json')
colordict = json.load(open(colors))
wal_foreground = colordict['special']['foreground']
wal_background = colordict['special']['background']
wal_cursor = colordict['special']['cursor']
wal_colors = [colordict['colors']['color' + str(i)] for i in range(16)]
```


### Utility Functions {#utility-functions}

I want to use the module [colorsys](https://docs.python.org/3/library/colorsys.html) to convert between different color systems. But this treats colors as tuples of floats between 0 and 1, whereas qtile treats them as hex strings. So we'll first need some conversion functions.

```python
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
```

What we want to do in the following is change the saturation and lightness of colors given to us by pywal. In the following function,

-   `col` is an RGB or RGBA color, either as a hex string, as a tuple or as a list
-   `value` represents a lightness or a saturation
    -   if `value` is a float between 0 and 1, then set the lightness/saturation of `col` to `value`
    -   if `value` is a string corresponding to a percentage, e.g. `"+10%"` or `"-5%"`, change the lightness/saturation accordingly
-   `i` is just a helper index, corresponding to the index in HLS format. So for lightness `i=1`, and for saturation `i=2`. It just exists so I don't have to write the same function twice.

<!--listend-->

```python
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
```


### Picking Out Some Nice Colors {#picking-out-some-nice-colors}

Now one thing I want is to pick out the color with the highest saturation (except from the background color). Or more generally, let's sort the colors by saturation.

```python
wal_colors_without_background = [col for col in wal_colors if col != wal_background]
wal_hls_without_background = [colorsys.rgb_to_hls(*string_to_rgb(col))
                              for col in wal_colors_without_background]

saturations = [colorsys.rgb_to_hls(*string_to_rgb(col))[2] for col in wal_colors_without_background]
wal_sorted_saturation = [pair[1] for pair in
                         sorted(zip(saturations, wal_colors_without_background),
                                key=lambda pair: -pair[0])]
```

Also it'd be cool to have access to the color which is most different in hue from the background color.

```python
wal_background_hls = colorsys.rgb_to_hls(*string_to_rgb(wal_background))
hue_differences = [abs(col[0] - wal_background_hls[0]) for col in wal_hls_without_background]
wal_sorted_huediff = [pair[1] for pair in
                         sorted(zip(hue_differences, wal_colors_without_background),
                                key=lambda pair: -pair[0])]
```

Another neat thing would be to have the background color at several different levels of lightness.

```python
wal_background_lightness = wal_background_hls[1]

# some magic numbers one can tweak
min_lightness = 0.5 * wal_background_lightness
max_lightness = min(2*wal_background_lightness,1)
num_lightness = 5

lightnesses = [min_lightness + i*(max_lightness-min_lightness)/(num_lightness-1)
               for i in range(num_lightness)]
wal_background_versions = [modify_lightness(wal_background,l) for l in lightnesses]
```


### Ok but what colors do we actually want to use? {#ok-but-what-colors-do-we-actually-want-to-use}

Now we finally set the colors used later in the config. For the primary color, let's lighten up the highest saturated color a bit.

```python
color_primary = modify_lightness(wal_sorted_saturation[0],0.7)
color_primary = modify_saturation(color_primary,"+20%")

color_secondary = modify_lightness(wal_sorted_saturation[1],0.85)
color_secondary = modify_saturation(color_secondary,"+20%")

bar_opacity = "C0"           # two hex digits
color_bar = wal_background + bar_opacity
```


## Random Config Variables {#random-config-variables}


### Settings {#settings}

```python
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
```


### Default Apps {#default-apps}

Setting up some default apps and commands to make future sections less cluttered. These are later run using `lazy.spawn()`, which does not quite work like a shell. E.g. it doesn't seem to support use of `&`, so a more elaborate syntax is needed in those cases.

```python
terminal = "kitty"
file_manager = "nemo"
web_browser = "firefox"
emacs = "emacsclient -c -a ''"
sysmon = terminal + " htop"
lockscreen = "xlock -mode \"rain\" -saturation 1 -mousemotion -timeout 10 -password \"Password please.\""
hibernate = ["sh", "-c", lockscreen + "& systemctl hibernate"]
suspend = ["sh", "-c", lockscreen + "& systemctl suspend"]
```


### Setup {#setup}

Let's also initialize some variables so we may simply add to them later.

```python
keys = []
```


## Layouts and Gaps {#layouts-and-gaps}

Note that a "layout" in qtile doesn't just talk about how your windows will appear on your screen. It also specifies certain aspects of how you move around windows. This should be kept in mind when picking what layouts you want. For more info, see the [built-in layouts documentation](https://docs.qtile.org/en/latest/manual/ref/layouts.html) as well as my comments about my keybindings below.

The layout also takes care of a couple of appearance options, like gaps and border colors. So these are also set here.

I don't need many layouts honestly. `MonadTall` does things exactly how I want it. `Max` is also nice for when I want just one big window, e.g. on my smaller laptop screen. I'll see if I'd also like `MonadWide`. And for floating windows, I'll just use a keybind to toggle floating. No need for an extra layout.

```python
layouts = [
    layout.MonadTall(
        border_width = 2,
        margin = 8,
        border_focus = color_primary,
        ),
    # layout.MonadWide(),
    layout.Max(),
]
```

I'll also set these options for the floating layout here since it fits here better, even though the actual implementation of these options appears only later on with the rules.

```python
floating_layout_theme = {"border_width": 2,
                "border_focus": color_secondary,
                "border_normal": wal_background}
```


## Groups {#groups}

In qtile, _groups_ are what would commonly be referred to as workspaces.

```python
groups = [Group(i) for i in "1234567890"]
```


### Multi-Monitor Behaviour {#multi-monitor-behaviour}

Qtile's default way of handling multiple monitors is inspired by xmonad. When trying to switch to switch to a group that is not currently active on any monitor, it will just show this group on the current monitor. Pretty standard stuff. The interesting thing happens when you try to switch to the group that is currently displayed on another monitor -- then it'll switch the two groups around.

While this is a very intriguing way to do things in theory, I could never quite get used to it in practice. So instead what I want instead: If the group to switch to is active on another monitor, just switch to that monitor and change nothing about the groups.

```python
def go_to_group(qtile,group_name):
    for s in qtile.screens:
        if s.group.name == group_name:
            qtile.cmd_to_screen(qtile.screens.index(s))
            return
    qtile.groups_map[group_name].toscreen()
```

This is nicely complemented by a function to switch the groups between monitors. Note that this just switches the group with the screen that comes one before it. If you have 3 or more monitors, you probably want something more sophisticated.

```python
def switch_screens(qtile):
    i = qtile.screens.index(qtile.current_screen)
    group = qtile.screens[i - 1].group
    qtile.current_screen.set_group(group)
```


### DGroups {#dgroups}

As I understand it, _dynamic groups_ are a way in qtile to dynamically create groups whenever they're needed. For example, you could have a new group be created everytime you launch your web browser, which will be removed once you close the webbrowser. Unfortunately, there is basically no documentation for this feature. Though I probably wouldn't use it anyway.

When using dynamic groups, the following variable could be used to automatically bind keybindings for newly created dgroups. I'll just disable it.

```python
dgroups_key_binder = None
```

We also don't need any rules for dgroups.

```python
dgroups_app_rules = []  # type: list
```


### Scratchpad {#scratchpad}

[Scratchpad](https://docs.qtile.org/en/latest/manual/config/groups.html#scratchpad-and-dropdown) groups are special groups that house dropdown windows. By default they are invisible, but the relevant dropdown windows can be made visible with the corresponding keybinding. I use this mainly to have a "quake-like" dropdown terminal available for when I quickly gotta execute some commands but don't want to leave the current workspace. Very convenient.

```python
groups.append(ScratchPad("scratchpad", [
        DropDown("quake_term", terminal, height=0.4, width=.995, x=.0025, y=0, opacity=0.9, on_focus_lost_hide=False),
]))
```


## Keybindings {#keybindings}

First of all, the most important keybinding. The modifier key. I use the super key, i.e. the "windows key".

```python
mod = "mod4"
```

A list of available commands that can be bound to keys can be found [in the docs](https://docs.qtile.org/en/latest/manual/config/lazy.html).


### Navigation {#navigation}

Something worth mentioning that was unusual to me when first switching to qtile: Many of the functions bound below do different depending on which layout you are in. For example, in the `Columns` layout (used in the default config), you would use `lazy.layout.left()`, `lazy.layout.right()`, `lazy.layout.up()`, `lazy.layout.down()` to move the focus between windows, much like e.g. in i3. On the other hand, in the `MonadTall` layout, `lazy.layout.up()` and `lazy.layout.down()` suffice and just move the focus to the _previous_ or _next_ window, instead of actually depending on the geometry of where windows are placed on the screen. I much prefer the latter way of navigating my windows. If you prefer the former, will probably want to change a lot of these keybindings.

```python
keys.extend([
    Key([mod], "j", lazy.layout.down(), desc="Move focus to next window"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus to previous window"),
])
```

Now, for moving windows. As mentioned above, I only really need `shuffle_up` and `shuffle_down` to move windows back and forth, but it can't hurt to also have bindings for left and right.

```python
keys.extend([
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
])
```

Finally, moving between monitors.

```python
keys.extend([
    Key([mod, "control"], "j", lazy.next_screen(), desc="Move focus to next monitor"),
    Key([mod, "control"], "k", lazy.prev_screen(), desc="Move focus to previous monitor"),
])
```


### Layout {#layout}

```python
keys.append(Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"))
```

We also wanna be able to grow and shrink windows. With these bindings I can only control the size of the main window. If I e.g. have three windows in the second column, I have no binding to make the center one larger than the other two. But that's good enough for me.

```python
keys.extend([
    Key([mod], "h", lazy.layout.shrink_main(), desc="Shrink window to the left"),
    Key([mod], "l", lazy.layout.grow_main(), desc="Shrink window to the right"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    Key([mod], "s", lazy.function(switch_screens), desc="Switch the groups on the two screens"),
])
```


### Modifying Window Properties {#modifying-window-properties}

```python
keys.extend([
    Key([mod], "f", lazy.window.toggle_fullscreen(), desc="Toggle fullscreen on the focused window",),
    Key([mod], "t", lazy.window.toggle_floating(), desc="Toggle floating on the focused window"),
    Key([mod], "m", lazy.window.toggle_minimize(), desc="Toggle Minimize Window"),
    Key([mod, "shift"], "m", lazy.window.unminimize_all(), desc="Unminimize all windows in group"),
])
```


### Ending It All {#ending-it-all}

```python
keys.extend([
    Key([mod, "shift"], "q", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, "shift"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "shift"], "e", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod, "control"], "l", lazy.spawn(lockscreen), desc="Lock screen"),
    Key([mod, "control"], "h", lazy.spawn(hibernate), desc="Hibernate"),
    Key([mod, "control"], "s", lazy.spawn(suspend), desc="Suspend system"),
    Key([mod, "control"], "X", lazy.spawn("xkill"), desc="xkill"),
])
```


### Starting It All {#starting-it-all}

```python
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
    Key([mod], "p", lazy.spawn("flameshot gui"), desc="Screenshot"),
    Key([mod, "control"], "w", lazy.spawn(os.path.expanduser("~/.config/change-wallpaper.sh")), desc="Change Wallpaper"),
])
```


### Groups {#groups}

The following adds the keybindings for groups whose names are only one character long. This character will be used in the keybinding.

For example: If you have a group name that is "webbrowser", no keybinding will be set. If you want the first group `i` to be designated for surfing the web, set `i.label` to be "webbrowser", but leave `i.name` as `1`.

```python
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
```


### Scratchpads {#scratchpads}

Keybindings for toggling dropdown windows defined [above.](#scratchpad)

```python
keys.extend([
        Key([mod, "control"], 'Return', lazy.group['scratchpad'].dropdown_toggle('quake_term')),
        ])
```


### Media and System Keys {#media-and-system-keys}

```python
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
```


### Mouse {#mouse}

Even though I'm a big fan of using the keyboard for almost everything, for floating windows a mouse is just sometimes more convenient.

```python
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]
```

Unfortunately [it seems like](https://github.com/qtile/qtile/issues/855) resizing _tiling_ windows with mouse is not really implemented into qtile yet. Oh well, there's worse things.


## Rules {#rules}


### Group Rules {#group-rules}

Rules that assign certain applications to certain groups. Keep in mind that python lists start at 0, so the numbers here have to be off by one.

```python
groups[8].matches = [Match(wm_class = "discord", title = "Discord Updater")]
groups[8].spawn = "discord"

groups[9].matches = [Match(wm_class = "spotify")]
```


### Floating Rules {#floating-rules}

Makes the following windows floating by default.

```python
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
```

The more complicated looking entry above is for the digital audio workstation Reaper. In a more regular desktop environment, all its popup windows will be small and floating, not full screen. This is a bit harder to achieve here since (using `xprop`) the popup windows don't really have any properties that sets them apart from the main window. I settled on differentiating between them by whether there is a version number in the title (i.e. whether `"REAPER v"` is a substring of the title).

In addition, let's change the default size of some windows.

```python
@hook.subscribe.client_managed
def resize_floating(client):
    if "emacs-everywhere" in client.name:
        client.set_size_floating(1200,700)
        client.center()
```


### On Screen Keyboard {#on-screen-keyboard}

On my convertible laptop, I use [Onboard](https://archlinux.org/packages/extra/x86_64/onboard/) as an onscreen keyboard. It has a neat autoshow feature as well as the ability to shrink other windows to accommodate the keyboard. Only trouble is that it sometimes ends up behind other floating windows. The following hook brings Onboard to the front every time it appears.

```python
if laptop:
        @hook.subscribe.client_managed
        def onboard_in_front(client):
                if client.name == "Onboard":
                        client.bring_to_front()
```


## Bar {#bar}

Default settings for all widgets.

```python
widget_defaults = dict(
    font="Ubuntu Nerd Font",
    fontsize=15,
    padding=3,
)
extension_defaults = widget_defaults.copy()
```

The following is the widget list for my bar (aka. panel). First we prepare the widget list, where some options depend on [desktop vs. laptop](#different-config-on-different-systems) use. The system tray widget is only allowed to be used once, so we need to create two widget lists for two screens: One with the systray, and one without it.

```python
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
```

Now we package these into bars.

```python
my_bars = [bar.Bar(
            init_widget_list(with_systray),
            size = 40,
            background = color_bar,
            opacity = 1, # but no transparency of widgets
            border_width = 0,
            reserve = True,
            #margin = [5, 5, -2, 5],
        ) for with_systray in [True, False]]
```

And finally, we put these on the screens.

```python
screens = [
    Screen(top=my_bars[0]),
    Screen(top=my_bars[1]),
]
```


## Autostart {#autostart}

Things are simpler if autostart is handled by a shell script rather than python. But by the power of org-babel, I can create this script from this org document!

```bash
#!/bin/bash

dunst &
picom -b --dbus &
blueman-applet &
dropbox start &
keepassxc &
nm-applet &
emacs --daemon &
touchegg &
setxkbmap de -variant nodeadkeys &
onboard &
```

Setting the keyboard layout is a workaround for a bug in Onboard that will change the layout of the on-screen keyboard while typing (cf. [here](https://www.reddit.com/r/linuxmint/comments/qonjkc/onboard_virtual_keyboard_switches_layout_while/) and [here](https://www.antixforum.com/forums/topic/onboard-does-not-work-with-azerty-keyboard/))

The hook causes the `autostart.sh` script to be executed once at startup.

```python
@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser('~/.config/qtile/autostart.sh')
    subprocess.Popen([home])
```


## Tangle this file! {#tangle-this-file}

Tangle on save? This hook will ask you after every save.

;; Local Variables:
;; eval: (add-hook 'after-save-hook (lambda ()(if (y-or-n-p "Tangle?")(org-babel-tangle))) nil t)
;; End:
