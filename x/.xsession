#! /bin/bash

## Torstein's .xinitrc/.xsession

## load x settings
x_resources="
  $HOME/.Xresources
  $HOME/src/my-little-friends/x/x-tango-theme
"
for x in ${x_resources}; do
  if [ -r "${x}" ]; then
    xrdb -merge "${x}"
  fi
done

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
clipit &> /dev/null &
conky &
firefox &> /dev/null &
emacs &>/dev/null &
mpd ~/.mpdconf &> /dev/null &
gnome-terminal &> /dev/null &
/opt/google/chrome/google-chrome \
  --profile-directory=Default \
  --app-id=bikioccmkafdpakkkcpdbppfkghcmihk &
/opt/google/chrome/google-chrome \
  --profile-directory=Default \
  --app-id=filhcaihcfanbgghhefcjhiccijdjbbd &

## set encoding before window manager starts
export LC_ALL=en_GB.utf8

## launch window manager
exec fluxbox -log ~/.fluxbox/log
# exec /usr/local/bin/fluxbox -log ~/.fluxbox/log
# exec mate-session
# exec gnome-session
# exec cinnamon-session
# exec startkde
# exec openbox
