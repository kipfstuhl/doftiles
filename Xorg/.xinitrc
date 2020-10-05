#!/bin/sh

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

# twm &
# xclock -geometry 50x50-1+1 &
# xterm -geometry 80x50+494+51 &
# xterm -geometry 80x20+494-0 &
# exec xterm -geometry 80x66+0+0 -name login

xbindkeys
# setxkbmap -rules xorg -layout "eu,de" -option "grp:alt_shift_toggle"
setxkbmap -layout "de,eu" -option "grp:alt_shift_toggle"

# activate screen locker
xset s 300 20
xss-lock -n /usr/lib/xsecurelock/dimmer -l -- env XSECURELOCK_SAVER=saver_xscreensaver xsecurelock &
#xss-lock -n /usr/lib/xsecurelock/dimmer -l -- xsecurelock &

# execute the profile
[ -f ~/.xprofile ] && . ~/.xprofile
exec awesome
