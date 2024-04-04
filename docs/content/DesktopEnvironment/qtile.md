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
from libqtile import bar, hook, layout, qtile, widget
from libqtile.config import Click, Drag, DropDown, Group, Key, Match, ScratchPad, Screen
from libqtile.lazy import lazy
import json
import os
import subprocess
```


## Wallpaper and Pywal colors {#wallpaper-and-pywal-colors}

I use [pywal](https://github.com/dylanaraps/pywal) to create colorschemes based on the current wallpaper. This lets me use these colors in qtile as well.

```python
process = subprocess.Popen('~/.config/change-wallpaper.sh', shell=True, stdout=subprocess.PIPE)
process.wait()

colors = os.path.expanduser('~/.cache/wal/colors.json')
colordict = json.load(open(colors))
wal_foreground = colordict['special']['foreground']
wal_background = colordict['special']['background']
wal_cursor = colordict['special']['cursor']
wal_colors = [colordict['colors']['color' + str(i)] for i in range(16)]
```

Somehow this runs the `change-wallpaper.sh` script twice, don't know why as of now.


## Random Config Variables {#random-config-variables}


### Settings {#settings}

```python
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
```


### Default Apps {#default-apps}

```python
terminal = "kitty"
file_manager = "nemo"
web_browser = "firefox"
sysmon = terminal + " htop"
```


### Setup {#setup}

Let's also initialize some variables so we may simply add to them later.

```python
keys = []
```


## Layouts and Appearance {#layouts-and-appearance}

Note that a "layout" in qtile doesn't just talk about how your windows will appear on your screen. It also specifies certain aspects of how you move around windows. This should be kept in mind when picking what layouts you want. For more info, see the [built-in layouts documentation](https://docs.qtile.org/en/latest/manual/ref/layouts.html) as well as my comments about my keybindings below.

The layout also takes care of a couple of appearance options, like gaps and border colors. So these are also set here.

I don't need many layouts honestly. `MonadTall` does things exactly how I want it. `Max` is also nice for when I want just one big window, e.g. on my smaller laptop screen. I'll see if I'd also like `MonadWide`. And for floating windows, I'll just use a keybind to toggle floating. No need for an extra layout.

```python
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
```

I'll also set these options for the floating layout here since it fits here better, even though the actual implementation of these options appears only later on with the rules.

```python
floating_layout_theme = {"border_width": 2,
                "border_focus": wal_colors[7],
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
        DropDown("quake_term", terminal, height=0.4, width=1, x=0, y=0, opacity=0.9, on_focus_lost_hide=False),
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


### Modyfing Window Properties {#modyfing-window-properties}

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
])
```


### Starting It All {#starting-it-all}

```python
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
    ],
)
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

The following is the widget list for my bar (aka. panel). The system tray widget is only allowed to be used once, so I need to create two widget lists for my two screens: One with the systray, and one without it.

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
```

Now we package these into bars.

```python
my_bars = [bar.Bar(
            init_widget_list(with_systray),
            size = 40,
            background = '#00000066', # transparent background
            opacity = 1, # but no transparency of widgets
            border_width = 0,
            reserve = True,
        ) for with_systray in [True, False]]
```

And finally, we put these on the screens.

```python
screens = [
    Screen(bottom=my_bars[0]),
    Screen(bottom=my_bars[1]),
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
```

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
