#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar
# If all your bars have ipc enabled, you can also use 
# polybar-msg cmd quit

# Launch bars
echo "---" | tee -a /tmp/polybar1.log /tmp/polybar2.log
polybar -c /home/reiti/.config/polybar/config main >>/tmp/polybar1.log 2>&1 & disown
polybar -c /home/reiti/.config/polybar/config tray >>/tmp/polybar2.log 2>&1 & disown

echo "Bars launched..."
