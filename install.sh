#!/usr/bin/env bash

#### INSTALL SCRIPT FOR MY SYSTEM ####
# The structure of this script is as follows: First I setup some variables and function.
# Then the script promtps you for all the parts you want to install. If you say yes, the
# relevant packages are added to a list and will be installed later. Even later, the dotfiles
# will be symlinked to appropriate directory (usually in $HOME/.config). If there is already
# a file or a directory where the symlink should be put, the script will print an error message
# and will not overwrite the existing files. (Exception is if the destination is an empty
# directoy - then it will be deleted and replaced by the symlink).

# This script only works on Arch based systems with the pacman package manager. Additionally the
# AUR helper yay as well as git is needed.

# To properly run this script, clone the git repo at github.com/freakybyte/dotfiles to some place
# you like, then execute the script. Read through the script first though pls, to make sure it's
# not gonna do anything you don't want. Also note that, as mentioned, the dotfiles will only be
# symlinked, not copied, so the clone of this repo can not be removed afterwards!


#### PREAMBLE ####
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

### Variables
PACMAN_PACKAGES=""
YAY_PACKAGES=""

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



#### Introduction to the Snow ####
### don't run as root
if [ `id -u` == 0 ]
  then echo -e "${PURPLE}Do not run this script as root/sudo!"
  exit
fi

echo -e "${RED}\n*** Do not blindly run this script! ***"
echo -e "${RESET}You don't know if you can trust me. And even if you believe me that this is not
going to install anything malicious, I might've just made a dumb mistake
somewhere in the script which is going to break your system. This script is
mostly there for my own comfort when having to set up a new system. Rather
than executing this script, I recommend you steal bits and pieces from my config
and read the script to learn about the installation."
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



#### SETUP ####
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

# initialize git submodules
echo -e "\nCloning git submodules..."
git submodule init
git submodule update



#### QUERIES ####
### Emacs
echo -e "${GREEN}\n"
read -p "Install Doom Emacs + config? [y/N] " -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
    INSTALL_EMACS=1
    PACMAN_PACKAGES="$PACMAN_PACKAGES git emacs-nativecomp findutils ripgrep fd"
fi

### Qtile
echo ""
read -p "Install Qtile and Co? [y/N] " -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
    INSTALL_QTILE=1
    PACMAN_PACKAGES="$PACMAN_PACKAGES qtile picom rofi dunst python-dbus-next breeze-icons xlockmore"
    YAY_PACKAGES="$YAY_PACKAGES light"
fi

### Terminal
echo ""
read -p "Install kitty? [y/N] " -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
    INSTALL_KITTY=1
    PACMAN_PACKAGES="$PACMAN_PACKAGES kitty"
fi

echo ""
read -p "Install fish? [y/N] " -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
    INSTALL_FISH=1
    PACMAN_PACKAGES="$PACMAN_PACKAGES fish eza"

    echo ""
    read -p "Install tide prompt for fish? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        INSTALL_TIDE=1
        PACMAN_PACKAGES="$PACMAN_PACKAGES fisher"
    fi
fi

echo ""
read -p "Install wallpaper change script ft. pywal? [y/N] " -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
    INSTALL_PYWAL=1
    PACMAN_PACKAGES="$PACMAN_PACKAGES python-pywal"
fi

echo -e "${RESET}\nBasically all of these configs use fonts that might not be installed on your
system. Without them, many things might not render correctly.${GREEN}"
read -p "Install fonts? [y/N] " -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
    INSTALL_FONTS=1
    PACMAN_PACKAGES="$PACMAN_PACKAGES ttf-ubuntu-font-family ttf-ubuntu-nerd ttf-jetbrains-mono ttf-jetbrains-mono-nerd"
fi


### Deprecated configs
echo -e "${RED}\n\n* DEPRECATED CONFIGS *"
echo -e "${RESET}The following configs are deprecated (see docs), but can be installed anyways if you want."

echo -e "${RED}No installer for i3 at this moment."

echo -e "${GREEN}"
read -p "Install awesome? [y/N] " -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
    INSTALL_AWESOME=1
    PACMAN_PACKAGES="$PACMAN_PACKAGES rofi picom inter-font acpi acpi_call acpid mpd mpc maim feh xclip xprop imagemagick blueman redshift xfce4-power-manager upower noto-fonts-emoji ttf-fantasque-nerd xdg-user-dirs iproute2 iw ffmpeg flat-remix"
    YAY_PACKAGES="$YAY_PACKAGES awesome-git light"
fi

echo ""
read -p "Install vim? [y/N] " -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
    INSTALL_VIM=1
    PACMAN_PACKAGES="$PACMAN_PACKAGES gvim"
    # gvim needed to make vim work with system clipboard, even though I use terminal vim
fi


### Other programs
echo -e "${BLUE}\n\n* OTHER PROGRAMS *${GREEN}"
read -p "Do you wish to install any extra programs? [y/N] " -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
    echo ""
    read -p "Install aspell (autocorrect with english and german dictionary)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES aspell aspell-de aspell-en"
    fi

    echo ""
    read -p "Install blueman (bluetooth manager)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES blueman"
    fi

    echo ""
    read -p "Install Breeze GTK theme and icons? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES breeze breeze-gtk breeze-icons"
    fi

    echo ""
    read -p "Install Discord? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        YAY_PACKAGES="$YAY_PACKAGES discord_arch_electron"
    fi

    echo ""
    read -p "Install Dropbox? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        YAY_PACKAGES="$YAY_PACKAGES dropbox"
    fi

    echo ""
    read -p "Install feh (minimal image viewer)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES feh"
    fi

    echo ""
    read -p "Install Firefox? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES firefox"
    fi

    echo ""
    read -p "Install Flameshot (screenshot tool)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES flameshot"
    fi

    echo ""
    read -p "Install Geogebra? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES geogebra"
    fi

    echo ""
    read -p "Install GIMP? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES gimp"
    fi

    echo ""
    read -p "Install Github Command Line Interface? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        INSTALL_GITHUB=1
        PACMAN_PACKAGES="$PACMAN_PACKAGES github-cli"
    fi

    echo ""
    read -p "Install Gparted? (disk partition manager) [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES gparted"
    fi

    echo ""
    read -p "Install gpick (gnome color picker)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES gpick"
    fi

    echo ""
    read -p "Install hugo (static website generator, used for the documentation of these dotfiles)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES gpick"
    fi

    echo ""
    read -p "Install joyutils (useful programs for dealing with joysticks)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES joyutils"
    fi

    echo ""
    read -p "Install KeepassXC (password manager)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES keepassxc"
    fi

    echo ""
    read -p "Install lxappearance (gui tool for setting gtk theme)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES lxappearance-gtk3"
    fi

    echo ""
    read -p "Install MAME (multiple arcade machine emulator)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES mame"
    fi

    echo ""
    read -p "Install Nemo (file browser)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES nemo"
    fi

    echo ""
    read -p "Install neofetch (displays system info)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES neofetch"
    fi

    echo ""
    read -p "Install nmapplet (gui applet for wifi)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES network-manager-applet"
    fi

    echo ""
    read -p "Install OBS (open broadcasting software)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES obs-studio"
    fi

    echo ""
    read -p "Install Onboard (onscreen keyboard)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES onboard"
        echo "    Install acpid and enable using systemd? [y/N]"
        read -p "    This is necessary for Onboard to be able to detect whether a convertible laptop is in tablet mode." -n 1 -r
        if [[ $REPLY =~ ^[Yy]$ ]]
        then
            ENABLE_ACPID=1
            PACMAN_PACKAGES="$PACMAN_PACKAGES acpid"
        fi
    fi

    echo ""
    read -p "Install pavucontrol (gui audio control center)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES pavucontrol"
    fi

    echo ""
    read -p "Install Spotify? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES spotify-launcher"
    fi

    echo ""
    read -p "Install TeX Live (LaTeX)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES texlive"
    fi

    echo ""
    read -p "Install Timeshift (backup utility)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        INSTALL_TIMESHIFT=1
        PACMAN_PACKAGES="$PACMAN_PACKAGES timeshift"
    fi

    echo ""
    read -p "Install Xournal++ (for taking handwritten notes)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES xournalpp"
    fi

    echo ""
    read -p "Install Zathura (pdf reader)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        PACMAN_PACKAGES="$PACMAN_PACKAGES zathura-djvu zathura-pdf-mupdf"
    fi

    echo ""
    read -p "Install Zoom (video conferences)? [y/N] " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        YAY_PACKAGES="$YAY_PACKAGES zoom"
    fi
fi



#### INSTALLATION ####
echo -e "\n\n${BLUE}* Installation will now begin. *${GREEN}"
read -p "Proceed? [y/N] " -n 1 -r
if [[ ! $REPLY =~ ^[Yy]$ ]]
then
    exit 1
fi

# Upgrade?
echo -e "${GREEN}\n"
read -p "Upgrade currently installed packages? [y/N] " -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]
then
    echo -e "${RESET}"
    sudo pacman -Syu --noconfirm
fi

# install earlier specified packages
if [ ! -z "$PACMAN_PACKAGES" ]
then
    echo -e "\n${BLUE}Installing packages using pacman...${RESET}\n"
    sudo pacman -S --needed --noconfirm $PACMAN_PACKAGES
fi
if [ ! -z "$YAY_PACKAGES" ]
then
    echo -e "\n${BLUE}Getting packages from the Arch User Repositories using yay...${RESET}\n"
    yay -S --needed --noconfirm $YAY_PACKAGES
fi


if [ ! -z "$INSTALL_EMACS" ]
then
    echo -e "\n${BLUE}Symlinking Emacs config...${RESET}\n"
    COMMAND="ln -s $SCRIPT_DIR/doom/ $HOME/.config/doom"
    do_if_doesnt_exist ~/.config/doom "Emacs configuration could not be installed." "$COMMAND"

    echo -e "\n${BLUE}Installing Doom...${RESET}\n"
    COMMAND="git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs &&
            ~/.config/emacs/bin/doom install && ~/.config/emacs/bin/doom sync && PATH=$PATH:~/.config/emacs/bin"
    do_if_doesnt_exist ~/.config/emacs "Doom could not be installed." "$COMMAND"
fi

if [ ! -z "$INSTALL_QTILE" ]
then
    echo -e "\n${BLUE}Symlinking Qtile config...${RESET}\n"
    COMMAND="ln -s $SCRIPT_DIR/qtile/ $HOME/.config/qtile"
    do_if_doesnt_exist ~/.config/qtile "Qtile configuration could not be installed." "$COMMAND"

    echo -e "\n${BLUE}Symlinking Picom config...${RESET}\n"
    COMMAND="ln -s $SCRIPT_DIR/picom.conf $HOME/.config/picom.conf"
    do_if_doesnt_exist ~/.config/picom.conf "Picom configuration could not be installed" "$COMMAND"

    echo -e "\n${BLUE}Symlinking Rofi config...${RESET}\n"
    COMMAND="ln -s $SCRIPT_DIR/rofi/ $HOME/.config/rofi"
    do_if_doesnt_exist ~/.config/rofi "Rofi configuration could not be installed" "$COMMAND"

    echo -e "\n${BLUE}Symlinking Dunst config...${RESET}\n"
    COMMAND="ln -s $SCRIPT_DIR/dunst/ $HOME/.config/dunst"
    do_if_doesnt_exist ~/.config/dunst "Dunst configuration could not be installed" "$COMMAND"
fi

if [ ! -z "$INSTALL_KITTY" ]
then
    echo -e "\n${BLUE}Symlinking kitty config...${RESET}\n"
    COMMAND="ln -s $SCRIPT_DIR/kitty/ $HOME/.config/kitty"
    do_if_doesnt_exist ~/.config/kitty "Kitty configuration could not be installed." "$COMMAND"
fi

if [ ! -z "$INSTALL_FISH" ]
then
    echo -e "\n${BLUE}Symlinking fish config...${RESET}\n"
    mkdir -p ~/.config/fish
    COMMAND="ln -s $SCRIPT_DIR/fish/config.fish $HOME/.config/fish/config.fish"
    do_if_doesnt_exist ~/.config/fish/config.fish "Fish configuration could not be installed." "$COMMAND"

    echo -e "\n${BLUE}Changing default shell to fish...${RESET}\n"
    chsh -s $(which fish)

    if [ ! -z "$INSTALL_EMACS" ]
    then
        echo -e "\n${BLUE}Adding doom binary to fish path...${RESET}\n"
        fish -c "fish_add_path ~/.config/emacs/bin"
    fi
fi
if [ ! -z "$INSTALL_TIDE" ]
then
    echo -e "\n${BLUE}Installing tide prompt...${RESET}\n"
    fish -c "fisher install IlanCosman/tide@v6"
fi

if [ ! -z "$INSTALL_PYWAL" ]
then
    echo -e "\n${BLUE}Symlinking pywal config...${RESET}\n"
    COMMAND="ln -s $SCRIPT_DIR/wal/ $HOME/.config/wal "
    do_if_doesnt_exist ~/.config/wal "Pywal configuration could not be installed" "$COMMAND"

    echo -e "\n${BLUE}Symlinking wallpaper change script...${RESET}\n"
    COMMAND="ln -s $SCRIPT_DIR/change-wallpaper.sh $HOME/.config/change-wallpaper.sh "
    do_if_doesnt_exist ~/.config/change-wallpaper.sh "Wallpaper change script could not be installed" "$COMMAND"

    if [ ! -d "~/Pictures/Wallpapers/used" ]; then
        echo -e "\n${BLUE}Wallpaper directory doesn't exist, creating...${RESET}\n"
        mkdir -p ~/Pictures/Wallpapers/used
    fi

    if [ -z "$(ls -A ~/Pictures/Wallpapers/used)" ]; then
        echo -e "\n${BLUE}No wallpapers found under ~/Pictures/Wallpapers/used \nDownloading example wallpaper...${RESET}\n"
        wget https://raw.githubusercontent.com/pablocorbalann/arch-minimal-wallpapers/main/wallpapers/full-hd/onedark.png -P ~/Pictures/Wallpapers/used/
    fi
fi

if [ ! -z "$INSTALL_FONTS" ]
then
    mkdir -p ~/.local/share/fonts    

    echo -e "\n${BLUE}Installing font Libertinus...${RESET}"
    wget -P /tmp/ https://github.com/alerque/libertinus/releases/download/v7.040/Libertinus-7.040.tar.xz
    tar -xf /tmp/Libertinus-7.040.tar.xz -C /tmp/
    cp /tmp/Libertinus-7.040/static/OTF/* ~/.local/share/fonts/
    rm /tmp/Libertinus-7.040.tar.xz
    rm -rf /tmp/Libertinus-7.040

    echo -e "\n${BLUE}Installing font K2D...${RESET}"
    git clone --depth 1 https://github.com/cadsondemak/K2D /tmp/K2D
    cp /tmp/K2D/fonts/* ~/.local/share/fonts/
    rm -rf /tmp/K2D

    echo -e "Updating font cache...${RESET}\n"
    fc-cache -f
fi

if [ ! -z "$INSTALL_AWESOME" ]
then
    echo -e "\n${BLUE}Symlinking awesome config...${RESET}\n"
    COMMAND="ln -s $SCRIPT_DIR/awesome/ $HOME/.config/awesome && cp ~/.config/awesome/configuration/config.lua.template ~/.config/awesome/configuration/config.lua"
    do_if_doesnt_exist ~/.config/awesome "Awesome configuration could not be installed." "$COMMAND"
fi

if [ ! -z "$INSTALL_VIM" ]
then
    echo -e "\n${BLUE}Symlinking vim config...${RESET}\n"
    COMMAND="ln -s $SCRIPT_DIR/.vimrc $HOME/.vimrc"
    do_if_doesnt_exist ~/.vimrc ".vimrc could not be installed" "$COMMAND"

    echo -e "\n${BLUE}Installing Vundle (vim plugin manager)...${RESET}"
    mkdir ~/.vim
    git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
    echo -e "${BLUE}Installing vim plugins...${RESET}"
    vim +PluginInstall +qall

    echo -e "\n${BLUE}Symlinking vim snippets...${RESET}\n"
    COMMAND="ln -s $SCRIPT_DIR/vim/UltiSnips/ $HOME/.vim/UltiSnips"
    do_if_doesnt_exist ~/.vim/UltiSnips "vim snippets could not be installed" "$COMMAND"
fi

if [ ! -z "$ENABLE_ONBOARD" ]
then
    echo -e "\n${BLUE}Enabling acpid...${RESET}\n"
    sudo systemctl enable acpid.service
    sudo systemctl start acpid.service
fi



echo -e "${BLUE}\n*** Done! Enjoy the config! :D ***\n${RESET}"
if [ ! -z "$INSTALL_GITHUB" ]
then
    echo -e "Github: You can now log into your account using:"
    echo -e "gh auth login"
    echo -e "\n Also don't forget to set your email and username if you haven't already:"
    echo -e "git config --global user.email \"my.address@email.com\""
    echo -e "git config --global user.name \"My_Edgy_Username_005\"\n"
fi
if [ ! -z "$INSTALL_TIMESHIFT" ]
then
    echo -e "Don't forget to set up your timeshift backups!"
fi
