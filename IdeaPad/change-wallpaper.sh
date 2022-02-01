#!/bin/bash

wal -i /home/reiti/Pictures/Wallpapers/landscape -n ${1: }
# optional parameter so I can e.g. pass the --iterative option

feh --bg-scale "$(< "${HOME}/.cache/wal/wal")" 	
# necessary cause somehow pywal doesn't change the wallpaper when used with awesome-wm

#/opt/spicetify-cli/spicetify apply
#wal-steam
