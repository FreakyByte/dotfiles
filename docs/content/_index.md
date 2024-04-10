+++
title = "Reitis Punktdateien"
author = ["Michael Reitmeir"]
tags = ["homepage"]
draft = false
+++

Henlo! I'm Michael, or FreakyByte, or reiti, or however you wanna call me. This is the documentation of my dotfiles, i.e. the configuration files for my Linux systems. Hope you find something you like!


## Components {#components}

My config roughly consists of the following parts:

1.  **The OS.** I use [EndeavourOS](https://endeavouros.com/). However, the only part where this is actually relevant is in the installation.
2.  **Emacs**. I use [Doom Emacs](https://github.com/doomemacs/doomemacs). This is not just an editor. My main use case is for all sorts of math/LaTeX stuff, including an elaborate note-taking setup using [org-roam](https://www.orgroam.com/). Other than that, I use it to write these nice configs and dock, and occasionally do some programming.
3.  **The Desktop Environment.**
    -   Main ingredient is the window manager [Qtile](https://qtile.org/). This is complimented by
    -   [picom](https://github.com/yshui/picom) as the compositor,
    -   [rofi](https://github.com/davatorium/rofi) as the application launcher,
    -   [dunst](https://github.com/dunst-project/dunst) as the notification daemon, and
    -   [pywal](https://github.com/dylanaraps/pywal) for some epic theming.
4.  **The Terminal.** Featuring
    -   [fish](https://fishshell.com/) as the shell, and
    -   [kitty](https://sw.kovidgoyal.net/kitty/) as the terminal emulator.


### Deprecated Components {#deprecated-components}

I also used and configured some other software previously. So there are old configs for them if you want to look at those, but I don't plan on improving these any further.

-   [vim](https://www.vim.org/) (though I still use it for some quick file editing)
-   [i3](https://i3wm.org/) + [polybar](https://github.com/polybar/polybar)
-   [awesomewm](https://awesomewm.org/)


## Installation {#installation}

The main reason I use Linux and all of this stuff is because I'm obsessed with customizing things (more reasons [here]({{< relref "whyyy" >}})). I therefore advocate you use my dotfiles as inspirations, stealing bits and pieces from them as you please.

If you do really want to _install_ some of my configs, I have an install script. However, **do not blindly run it**, and check first whether it won't do anything you don't want. It also relies on the pacman package manager, so it only works on Arch based distros. To run the script, first clone the git repo of my dotfiles using

```bash
git clone --depth 1 https://github.com/FreakyByte/dotfiles
```

After that, executing \`install.sh\` will prompt you which parts of my config you want, install the relevant packages and symlink the relevant config files. Note that symlinking means the cloned repo can not be removed afterwards. If you want that, you have to copy the files yourself.

If you're using a stable release distribution like e.g. Ubuntu, keep in mind that the versions of programs in the official repos might be too old to work with these dotfiles. In this case, you'll have to manually build the newer packages yourself or use a [PPA](https://help.ubuntu.com/community/PPA) or something.
