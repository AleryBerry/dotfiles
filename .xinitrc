#!/bin/bash

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi
if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi
if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi
if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi
# start some nice programs
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi
exec nitrogen --restore &
exec clipmenud &
exec picom &
exec lightscreen -i &

outputs=$(xrandr --query | grep " connected" | cut -d" " -f1)
tray_output=eDP1

for m in $outputs; do
  if [[ $m == "DVI-D-1" ]]; then
      tray_output=$m
  fi
done

for m in $outputs; do
  export MONITOR=$m
  if [[ $m =~ ([a-zA-Z]+) ]]; then export MONIT=$BASH_REMATCH; fi
  export TRAY_POSITION=none
  if [[ $m == $tray_output ]]; then
    TRAY_POSITION=right
  fi
  exec polybar bar & 
done

exec ~/.local/bin/xmonad

