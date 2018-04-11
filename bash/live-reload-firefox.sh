#!/bin/bash

# Reloads the current Firefox tab when executed and returns to Emacs afterwards.

# Variables
firefoxWindow=$(xdotool search --name "Mozilla Firefox")
# GUI Emacs
emacsWindow=$(xdotool search --name "emacs24@")

xdotool windowactivate $firefoxWindow
# There's a delay in xdotool, hence this sleep
sleep 0.5
xdotool key F5

# Talk to me
notify-send -t 3000 "Viewing page, will return to Emacs in 6s"
sleep 3
notify-send -t 3000 "Returning to Emacs..."
sleep 3

# Take me home
xdotool windowactivate $emacsWindow
