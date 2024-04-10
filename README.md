# Reitis wilde Punktdateien

Henlo! I'm Michael, or FreakyByte, or reiti, or however you wanna call me. These are my dotfiles, i.e. the configuration files for my Linux systems. Hope you find something you like!

## Short Description
I use:
- [EndeavourOS](https://endeavouros.com/) as my Linux distro (so I kinda use arch, I guess)
- [Doom Emacs](https://github.com/doomemacs/doomemacs) as my, uh, "editor"
- [Qtile](https://qtile.org/) as my window manager, including
  - [picom](https://github.com/yshui/picom) as my compositor
  - [rofi](https://github.com/davatorium/rofi) as my application launcher
  - [dust](https://github.com/dunst-project/dunst) as my notification-daemon
  - [pywal](https://github.com/dylanaraps/pywal) for some epic theming
- [fish](https://fishshell.com/) as my shell
- [kitty](https://sw.kovidgoyal.net/kitty/) as my terminal emulator

Previously I also extensively used the following. So I have old configurations for them, but I don't intend on improving them any further.
- [vim](https://www.vim.org/) as text-editor - though I still use it for some quick file editing. I just mostly switched to Emacs.
- [i3](https://i3wm.org/) + [polybar](https://github.com/polybar/polybar) as window manager
- [awesome](https://awesomewm.org/) as window manager (inspired by [the-glorious-dotfiles](https://github.com/eromatiya/the-glorious-dotfiles))

I run this config on two machines: A desktop PC (called "One") that I use for work as well as entertainment, and a convertible laptop (called "IdeaPad") which I mostly use for uni. Some parts of the config (mainly the window managers) vary between the two systems. For more details, check the docs.

## Documentation
Rambling and explaining stuff is fun. I'm a fan of literate configs, meaning writing the documentation/explanation and the code itself directly next to each other. These and other docs are hosted on the [Github pages for this repo](https://freakybyte.github.io/dotfiles/). 

The docs are generated using [hugo](https://gohugo.io/) + [ox-hugo](https://ox-hugo.scripter.co/) and use the [relearn](https://mcshelby.github.io/hugo-theme-relearn/) theme.

## Installation
The main reason I use Linux and all of this stuff is because I'm obsessed with customizing things. Therefore I advocate you use my dotfiles as inspirations, stealing bits and pieces from them as you please.

If you do really want to install some of my configs, I have an install script. However, **do not blindly run it**, and check first whether it won't do anything you don't want. It also relies on the pacman package manager, so it only works on Arch based distros. To run the script, first clone this repo using
`git clone --depth 1 https://github.com/FreakyByte/dotfiles`
After that, executing `install.sh` will prompt you which parts of my config you want, install the relevant packages and symlink the relevant config files. Note that symlinking means the cloned repo can not be removed afterwards. If you want that, you have to copy the files yourself.

If you're using a stable release distribution like e.g. Ubuntu, keep in mind that the versions of programs in the official repos might be too old to work with these dotfiles. In this case, you'll have to manually build the newer packages yourself or use a [PPA](https://help.ubuntu.com/community/PPA) or something.
