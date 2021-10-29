#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar
# If all your bars have ipc enabled, you can also use 
# polybar-msg cmd quit

# Find rotation to find out which bar should be drawn
rotation="$(xrandr -q --verbose | grep 'connected' | egrep -o  '\) (normal|left|inverted|right) \(' | egrep -o '(normal|left|inverted|right)')" 

# Launch bars
echo "---" | tee -a /tmp/polybar1.log /tmp/polybar2.log
case "$rotation" in 
    normal) 
    polybar -c /home/reiti/.config/polybar/config main >>/tmp/polybar1.log 2>&1 & disown
    polybar -c /home/reiti/.config/polybar/config tray >>/tmp/polybar2.log 2>&1 & disown
	;;
    left) 
    polybar -c /home/reiti/.config/polybar/config short >>/tmp/polybar1.log 2>&1 & disown
    polybar -c /home/reiti/.config/polybar/config tray2 >>/tmp/polybar2.log 2>&1 & disown
	;; 
    inverted) 
    polybar -c /home/reiti/.config/polybar/config main >>/tmp/polybar1.log 2>&1 & disown
    polybar -c /home/reiti/.config/polybar/config tray >>/tmp/polybar2.log 2>&1 & disown
	;;
    right) 
    polybar -c /home/reiti/.config/polybar/config short >>/tmp/polybar1.log 2>&1 & disown
    polybar -c /home/reiti/.config/polybar/config tray2 >>/tmp/polybar2.log 2>&1 & disown
	;; 
esac

echo "Bars launched..."
