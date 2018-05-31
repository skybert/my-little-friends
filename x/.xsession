#! /bin/bash

## Torstein's .xinitrc/.xsession

wm=/usr/local/bin/i3

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

start_signal_and_slack() {
  /opt/google/chrome/google-chrome \
    --profile-directory=Default \
    --app-id=bikioccmkafdpakkkcpdbppfkghcmihk &
  /opt/google/chrome/google-chrome \
    --profile-directory=Default \
    --app-id=filhcaihcfanbgghhefcjhiccijdjbbd &
}


## hide mouse pointer after a couple of seconds
unclutter -idle 2 &

## make caps lock act as control
setxkbmap -option ctrl:nocaps &

## disable touch pad while typing
syndaemon -t -k -i 2 &

## turn off the stupid bell, why oh why isn't this the default?
xset -b

## set background image
tkj set-wallpaper &

## favourite apps
clipit &> /dev/null &
firefox &> /dev/null &
emacs &>/dev/null &
mpd ~/.mpdconf &> /dev/null &
gnome-terminal &> /dev/null &
# start_signal_and_slack

## set encoding before window manager starts
export LC_ALL=en_GB.utf8

## launch window manager
exec "${wm}"
