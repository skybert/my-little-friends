#! /bin/sh

## Torstein's .xinitrc/.xsession

## load x settings
xrdb $HOME/.Xresources

## remap keyboard
if [ -r "$HOME/.xmodmaprc" ]; then
  xmodmap "$HOME/.xmodmaprc"
fi

## hide mouse pointer after a couple of seconds
unclutter -idle 2 &

## make caps lock act as control
setxkbmap -option ctrl:nocaps &

## disable touch pad while typing
syndaemon -t -k -i 2 &

## set background image
if [ -e $HOME/pictures/wallpaper ]; then
  display -backdrop -window root $HOME/pictures/wallpaper &
fi

## favourite apps
clipit &
conky &
vivaldi &
emacs-snapshot -fn 7x14 -g 80x60 &
mpd $HOME/.mpdconf &
gnome-terminal &

## set encoding before window manager starts
export LC_ALL=en_GB.utf8

## launch window manager
# exec fluxbox -log ~/.fluxbox/log
exec /usr/local/bin/fluxbox -log ~/.fluxbox/log
# exec mate-session
# exec gnome-session
# exec cinnamon-session
# exec startkde

