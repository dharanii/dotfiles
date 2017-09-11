#!/usr/bin/env bash

export DUNST_FONT='Artwiz Lemon 7'
export DUNST_SIZE='300x30-40+70'

xscreensaver &
xsetroot -cursor_name left_ptr &

compton --config /dev/null &
dunst -fn "$DUNST_FONT" -geometry "$DUNST_SIZE" &
urxvtd -q -f &

xrdb -merge -I$HOME ~/.Xresources
wal -R -o wal-set

exec xmonad 
