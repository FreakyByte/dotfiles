+++
title = "Awesome"
author = ["Michael Reitmeir"]
draft = false
weight = 2
+++

The [awesome](https://awesomewm.org/) window manager lives up to its name. It is an awesome window manager, and I really enjoyed using it for like 2 to 3 years!


## The Glorious Dotfiles {#the-glorious-dotfiles}

My awesome config is basically the [floppy theme](https://github.com/eromatiya/the-glorious-dotfiles/tree/master/config/awesome/floppy) from [the glorious dotfiles](https://github.com/eromatiya/the-glorious-dotfiles). Quite an elaborate setup, closer to an actual desktop environment than a window manager. They are unfortunately deprecated since 2020 (so already have been when I started using it - I guess I was hoping ^^).

I'm not gonna do a complete documentation on them here, that'd be _a lot_ of work. Instead, check out the original [wiki](https://github.com/eromatiya/the-glorious-dotfiles/wiki).


### My Changes {#my-changes}

Among others, my changes include:

-   fixed an issue resulting from a newer Lua version, see [here](https://github.com/awesomeWM/awesome/issues/3563#issuecomment-1036000769)
-   made left sidebar hideable (and hidden by default), moving the tag-list to the top bar
-   using [Charitable](https://github.com/frioux/charitable) to imitate the way multiple monitors are handled in [xmonad](https://xmonad.org/)
-   changing a couple icons to the Flat-Remix icon theme
-   changing the rofi appmenu to the one from <https://github.com/elenapan/dotfiles>
-   using pywal for wallpapers and colorscheme
-   added variable to `config.lua` to make different settings for my two machines possible
-   just general customization of tags and stuff


### Two different Setups {#two-different-setups}

In order to have different configs on my desktop ("One") and laptop ("IdeaPad") like showing/hiding the battery status etc., there is a variable in the file `awesome/configuration/config.lua` that needs to be set. To have this variable change (and other private options) not synced to my Github, this file is put in `.gitignore`, and a template file `awesome/configuration/config.lua.template` is provided instead. So what you gotta do is:

```bash
mv awesome/configuration/config.lua.template awesome/configuration/config.lua
```

Then configure the variable `system` in `awesome/configuration/config.lua` to the one you want:

-   If you want the desktop config: set `system = One` (default)
-   If you want the laptop config: set `system = Ideapad`

Also configure the other variables in that file, like username, email, OpenWeather API key, etc.


### Planned Changes {#planned-changes}

I also had a bunch of other changes planned that I never had the time to implement:

-   more configuration of the side bar menu, to do things like moving windows between tags etc. from a touchscreen
-   make window titlebars toggleable - I kinda started doing it, but right now if you disable them and open a new window, it's gonna have them enabled again and it's a whole mess
-   configure Desktop right-click menu, possibly as an app launcher for touchscreen
-   add list of running programs to statusbar (e.g. for unminimizing windows through the GUI)


## Dependencies {#dependencies}

Also mostly taken from [here](https://github.com/manilarome/the-glorious-dotfiles/wiki#dependencies), but some things were added/removed for this config.

Required dependencies:

| Name                                               | Description                                                 | Why/Where is it needed?                          |
|----------------------------------------------------|-------------------------------------------------------------|--------------------------------------------------|
| [awesome](https://github.com/awesomeWM/awesome)    | Highly configurable framework window manager                | Main component                                   |
| [rofi](https://github.com/davatorium/rofi)         | Window switcher, application launcher and dmenu replacement | application launcher                             |
| [picom](https://github.com/yshui/picom)            | A compositor for X11                                        | transparency and blur (specifically kawase-blur) |
| [inter-font](https://github.com/rsms/inter/)       | A typeface specially designed for user interfaces           | Setup font                                       |
| [charitable](https://github.com/frioux/charitable) | Shared tags library for multiple monitors using AwesomeWM   | makes multimonitor work like in xmonad           |

Optional dependencies:

| Name                                                 | Description                                                     | Will be used by                                                      |
|------------------------------------------------------|-----------------------------------------------------------------|----------------------------------------------------------------------|
| light-git                                            | RandR-based backlight control application                       | Brightness widget and OSD                                            |
| alsa-utils, pulseaudio, pulseaudio-alsa              | An alternative implementation of Linux sound support            | Volume widget and OSD                                                |
| acpi, acpid, acpi_call                               | Show battery status and other ACPI info                         | Power/Battery Widgets. No need for this if you're not using a laptop |
| mpd                                                  | Server-side application for playing music                       | Music widget                                                         |
| mpc                                                  | Minimalist command line interface to MPD                        | Music widget                                                         |
| maim                                                 | Make image                                                      | Screenshot tool                                                      |
| feh                                                  | Image viewer and wallpaper setter                               | Screenshot previews, wallpapers                                      |
| xclip                                                | Command line interface to the X11 clipboard                     | Will be used in saving the screenshots to clipboard                  |
| xprop                                                | Property displayer for X                                        | Custom titlebars for each client                                     |
| imagemagick                                          | An image viewing/manipulation program                           | Music widget/Extracts hardcoded album cover from songs               |
| blueman                                              | Manages bluetooth                                               | default launch application for bluetooth widget                      |
| redshift                                             | Sets color temperature of display according to time of day      | Blue light widget                                                    |
| xfce4-power-manager                                  | Manages power                                                   | default launch application for battery widget                        |
| upower                                               | upower - UPower command line tool                               | Battery widget                                                       |
| noto-fonts-emoji                                     | Google Noto emoji fonts                                         | Emoji support for notification center                                |
| FantasqueSansMono Nerd Font                          | Patched font FantasqueSansMono from the nerd-fonts library      | Rofi unicode font                                                    |
| xdg-user-dirs                                        | Manage user directories like ~/Desktop and ~/Music              | xdg-folders widget                                                   |
| iproute2, iw                                         | Manage network connection                                       | Network widget                                                       |
| ffmpeg                                               | Complete solution to record, convert and stream audio and video | Screen Recorder Widget                                               |
| [pywal](https://github.com/dylanaraps/pywal)         | generates a color palette from the dominant colors in an image  | theme terminals, emacs, etc. with colors of wallpaper                |
| [Flat-Remix](https://github.com/daniruiz/flat-remix) | icon theme inspired by material design                          | icons in multiple places                                             |


## Reasons why I switched {#reasons-why-i-switched}

Even though I really liked awesome, I switched to qtile in 2024. Here's a short summary why:

-   I wanted to change a bunch of things, and doing so in this big elaborate config that I didn't properly document and didn't really fully remember the structure of seemed daunting. So I wanted to start over with a config of my own.
-   I know a little bit of Python, but the tiny bit of Lua I know I learned from configuring this in 2021 or something. So switching to a window manager where I can use python seemed simpler.
-   Qtile also just seemed simpler in general, making a more clear and minimal config possible (though maybe I am biased by using a big awesome config I didn't fully understand).
-   I discovered the widget system [eww](https://github.com/elkowar/eww), which I wanted to switch to, especially for touch screen stuff (still a big work in progress ^^). So I didn't need the broad widgeting capabilities of awesome anymore.
-   Awesome has no plans to be ported to [Wayland](https://en.wikipedia.org/wiki/Wayland_(protocol)), whereas qtile already has a Wayland version. (This is a very minor reason for me though. At the time of writing this, I'm using qtile on X11.)
