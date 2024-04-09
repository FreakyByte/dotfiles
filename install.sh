#!/usr/bin/env bash

### Colors
BLACK='\e[0;30m'
RED='\e[0;31m'
GREEN='\e[0;32m'
YELLOW='\e[0;33m'
BLUE='\e[0;34m'
PURPLE='\e[0;35m'
CYAN='\e[0;36m'
WHITE='\e[0;37m'
RESET='\033[0m'

### Functions
remove_if_empty () {
    # remove $1 iff it is an empty directory
    if [ -d $1 ]; then
        if [ -z "$(ls -A $1)" ]; then
            rmdir $1
        fi
    fi
}

do_if_doesnt_exist () {
    remove_if_empty $1
    if [ -e $1 ]; then
        # if $1 still exists, display error message
        echo -e "${PURPLE}$1 already exists!"
        echo -e "$2"
        echo -e "${RESET}"
    else
        # otherwise, execute command
        eval "$3";
    fi
}



### don't run as root
if [ `id -u` == 0 ]
  then echo -e "${PURPLE}Do not run this script as root/sudo!"
  exit
fi


### Introduction to the Snow
echo -e "${RED}\n*** WARNING - Do not blindly run this script! ***"
echo -e "${RESET}You don't know if you can trust me. And even if you believe me that this is not
going to install anything malicious, I might've just made a dumb mistake
somewhere in the script which is going to break your system. Rather, what you
should do is read this script and execute only the parts that you actually need."
echo -e "${GREEN}"
read -p "Execute anyways? [y/N] " -n 1 -r
if [[ ! $REPLY =~ ^[Yy]$ ]]
then
    exit 1
fi

echo -e "${RESET}"
echo -e "Alright then. But at least make sure the following things are true,
otherwise most things in this script will not work!
    - I use Arch, btw. Or any distro using the pacman package manager.
    - yay is installed as the AUR helper."

echo -e "${GREEN}"
read -p "Proceed? [y/N] " -n 1 -r
if [[ ! $REPLY =~ ^[Yy]$ ]]
then
    exit 1
fi

echo -e "${RESET}\n"

### SETUP
if ! command -v pacman &> /dev/null
then
    echo -e "${PURPLE}pacman could not be found, exiting"
    exit 1
fi
if ! command -v yay &> /dev/null
then
    echo -e "${PURPLE}yay could not be found, exiting"
    exit 1
fi

# change directory
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR

echo -e "A bunch of things in this script will need sudo access."
sudo -v

# initialize git submodules
echo -e "\nCloning git submodules..."
git submodule init
git submodule update

### Upgrade?
echo -e "${GREEN}\n"
read -p "Upgrade currently installed packages? [y/N] " -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
    echo -e "${RESET}"
    sudo pacman -Syu --noconfirm
fi


### Essentials
#echo -e "${GREEN}\n"
#read -p "Install essential packages like git? [y/N] " -n 1 -r
#if [[ $REPLY =~ ^[Yy]$ ]]
#then
#    echo -e "${RESET}"
#    sudo pacman -S --needed --noconfirm git
#fi


### Emacs
echo -e "${GREEN}\n"
read -p "Install Doom Emacs + config? [y/N] " -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
    echo -e "${RESET}"
    # emacs for X11 built with native compilation
    sudo pacman -S --needed --noconfirm emacs-nativecomp
    # dependencies for doom
    sudo pacman -S --needed --noconfirm git findutils ripgrep fd

    COMMAND="ln -s $SCRIPT_DIR/doom/ $HOME/.config/doom"
    do_if_doesnt_exist ~/.config/doom "Emacs configuration could not be installed." $COMMAND

    COMMAND="git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs &&
            ~/.config/emacs/bin/doom install"
    do_if_doesnt_exist ~/.config/emacs "Doom could not be installed." $COMMAND
fi


### Qtile
echo -e "${GREEN}\n"
read -p "Install Qtile and Co? [y/N] " -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
    echo -e "${RESET}"
    sudo pacman -S --needed --noconfirm qtile picom rofi dunst python-pywal feh

    COMMAND="ln -s $SCRIPT_DIR/qtile/ $HOME/.config/qtile"
    do_if_doesnt_exist ~/.config/qtile "Qtile configuration could not be installed." $COMMAND

    COMMAND="ln -s $SCRIPT_DIR/picom.conf $HOME/.config/picom.conf"
    do_if_doesnt_exist ~/.config/picom.conf "Picom configuration could not be installed" $COMMAND

    COMMAND="ln -s $SCRIPT_DIR/rofi/ $HOME/.config/rofi"
    do_if_doesnt_exist ~/.config/rofi "Picom configuration could not be installed" $COMMAND

    COMMAND="ln -s $SCRIPT_DIR/dunst/ $HOME/.config/dunst"
    do_if_doesnt_exist ~/.config/dunst "Dunst configuration could not be installed" $COMMAND

    COMMAND="ln -s $SCRIPT_DIR/wal/ $HOME/.config/wal && PYWAL_INSTALLED=1"
    do_if_doesnt_exist ~/.config/wal "Pywal configuration could not be installed" $COMMAND
fi


### Terminal
echo -e "${GREEN}\n"
read -p "Install kitty? [y/N] " -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
    echo -e "${RESET}"
    sudo pacman -S --needed --noconfirm kitty

    COMMAND="ln -s $SCRIPT_DIR/kitty/ $HOME/.config/kitty"
    do_if_doesnt_exist ~/.config/kitty "Kitty configuration could not be installed." $COMMAND
fi

echo -e "${GREEN}\n"
read -p "Install fish? [y/N] " -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
    echo -e "${RESET}"
    sudo pacman -S --needed --noconfirm fish
    yay -S --needed --noconfirm eza

    COMMAND="ln -s $SCRIPT_DIR/fish/config.fish $HOME/.config/fish/config.fish"
    do_if_doesnt_exist ~/.config/fish/config.fish "Fish configuration could not be installed." $COMMAND


    echo -e "${GREEN}\n"
    read -p "Install tide prompt for fish? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm fisher && fisher install IlanCosman/tide@v6
    fi
fi

if [ -z "$PYWAL_INSTALLED" ]
then  # if pywal has not been installed in previous step
    echo -e "${GREEN}\n"
    read -p "Install pywal? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm python-pywal

        COMMAND="ln -s $SCRIPT_DIR/wal/ $HOME/.config/wal && PYWAL_INSTALLED=1"
        do_if_doesnt_exist ~/.config/wal "Pywal configuration could not be installed" $COMMAND
    fi
fi


### Fonts
echo -e "${RESET}\nBasically all of these configs use fonts that might not be installed on your
system. Without them, many things might not render correctly.${GREEN}"
read -p "Install fonts? [y/N] " -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
    echo -e "${RESET}"
    sudo pacman -S --needed --noconfirm ttf-ubuntu-font-family ttf-ubuntu-nerd ttf-jetbrains-mono ttf-jetbrains-mono-nerd

    echo -e "Installing font Libertinus..."
    wget -P /tmp/ https://github.com/alerque/libertinus/releases/download/v7.040/Libertinus-7.040.tar.xz
    tar -xf /tmp/Libertinus-7.040.tar.xz -C /tmp/
    cp /tmp/Libertinus-7.040/static/OTF/* ~/.local/share/fonts/
    rm /tmp/Libertinus-7.040.tar.xz
    rm -rf /tmp/Libertinus-7.040

    echo -e "Installing font K2D..."
    git clone --depth 1 https://github.com/cadsondemak/K2D /tmp/K2D
    cp /tmp/K2D/fonts/* ~/.local/share/fonts/
    rm -rf /tmp/K2D

    echo -e "Updating font cache..."
    fc-cache -f
fi


### Deprecated configs
echo -e "${RED}\n* DEPRECATED CONFIGS *"
echo -e "${RESET}The following configs are deprecated (see docs), but can be installed anyways if you want."

echo -e "${RED}\nNo installer for i3 at this moment."

echo -e "${GREEN}"
read -p "Install awesome? [y/N] " -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
    echo -e "${RESET}"
    yay -S --needed --noconfirm awesome-git light
    sudo pacman -S --needed --noconfirm rofi picom inter-font acpi acpi_call acpid mpd mpc maim feh xclip xprop imagemagick blueman redshift xfce4-power-manager upower noto-fonts-emoji ttf-fantasque-nerd xdg-user-dirs iproute2 iw ffmpeg flat-remix

    COMMAND="ln -s $SCRIPT_DIR/awesome/ $HOME/.config/awesome && cp ~/.config/awesome/configuration/config.lua.template ~/.config/awesome/configuration/config.lua"
    do_if_doesnt_exist ~/.config/kitty "Awesome configuration could not be installed." $COMMAND
fi



### Other programs
echo -e "${BLUE}\n* OTHER PROGRAMS *"
echo -e "${GREEN}"
read -p "Do you wish to install any extra programs? [y/N] " -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
    read -p "\n\nInstall aspell (autocorrect with english and german dictionary)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm aspell aspell-de aspell-en
    fi

    read -p "\n\nInstall blueman (bluetooth manager)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm blueman
    fi

    read -p "\n\nInstall Breeze GTK theme and icons? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm breeze breeze-gtk breeze-icons
    fi

    read -p "\n\nInstall Discord? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        yay -S --needed --noconfirm discord_arch_electorn
    fi

    read -p "\n\nInstall Dropbox? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        yay -S --needed --noconfirm dropbox
    fi

    read -p "\n\nInstall feh (minimal image viewer)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm feh
    fi

    read -p "\n\nInstall Firefox? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm firefox
    fi

    read -p "\n\nInstall Flameshot (screenshot tool)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm flameshot
    fi

    read -p "\n\nInstall Geogebra? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm geogebra
    fi

    read -p "\n\nInstall GIMP? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm gimp
    fi

    read -p "\n\nInstall GIMP? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm github-cli
    fi

    read -p "\n\nInstall Gparted? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm gparted
    fi

    read -p "\n\nInstall gpick (gnome color picker)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm gpick
    fi

    read -p "\n\nInstall joyutils (useful programs for dealing with joysticks)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm joyutils
    fi

    read -p "\n\nInstall joyutils (useful programs for dealing with joysticks)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm gpick
    fi

    read -p "\n\nInstall KeepassXC (password manager)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm keepassxc
    fi

    read -p "\n\nInstall lxappearance (gui tool for setting gtk theme)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm lxappearance-gtk3
    fi

    read -p "\n\nInstall MAME (multiple arcade machine emulator)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm mame
    fi

    read -p "\n\nInstall Nemo (file browser)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm nemo
    fi

    read -p "\n\nInstall neofecth (displays system info)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm neofetch
    fi

    read -p "\n\nInstall nmapplet (gui applet for wifi)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm network-manager-applet
    fi

    read -p "\n\nInstall OBS (open broadcasting software)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm obs-studio
    fi

    read -p "\n\nInstall pavucontrol (gui audio control center)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm pavucontrol
    fi

    read -p "\n\nInstall Spotify? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm spotify-launcher
    fi

    read -p "\n\nInstall TeX Live (LaTeX)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm texlive
    fi

    read -p "\n\nInstall Timeshift (backup utility)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm texlive
    fi

    read -p "\n\nInstall Zathura (pdf reader)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        sudo pacman -S --needed --noconfirm zathura-djvu zathura-pdf-mupdf
    fi

    read -p "\n\nInstall Zoom (online conferences)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo -e "${RESET}"
        yay -S --needed --noconfirm zoom
    fi
fi
