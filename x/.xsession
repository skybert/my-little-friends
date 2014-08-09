#! /bin/sh

## Torstein's .xinitrc/.xsession

## load x settings
xrdb $HOME/.Xresources

## remap keyboard
xmodmap "$HOME/.xmodmaprc"

## hide mouse pointer after a couple of seconds
# unclutter -idle 2 &

## make caps lock act as control
setxkbmap -option ctrl:nocaps &

## disable touch pad while typing
syndaemon -t -k -i 2 &

## set background image
display -backdrop -window root ~/pictures/wallpaper &

## favourite apps
clipit &
conky &
firefox &
xmms &
emacs -fn 7x14 -g 80x60 &

## set encoding before window manager starts
export LC_ALL=en_GB.utf8

## launch window manager
exec fluxbox

