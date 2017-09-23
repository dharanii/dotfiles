#!/usr/bin/env bash

export DUNST_FONT='Artwiz Lemon 7'
export DUNST_SIZE='300x30-40+70'

export GTK_IM_MODULE='fcitx'
export QT_IM_MODULE='fcitx'
export XMODIFIERS=@im='fcitx'

compton --config /dev/null &
pulseaudio --start &
fcitx-autostart &
dunst -fn "$DUNST_FONT" -geometry "$DUNST_SIZE" &
urxvtd -q -f &

wal -R -o wal-set

xrdb -merge "${HOME}/.Xresources" &
xsetroot -cursor_name left_ptr &
xautolock -time 3 -locker lock &

exec xmonad 
