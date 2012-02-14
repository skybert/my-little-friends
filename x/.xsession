#! /bin/sh

# Torstein's .xinitrc/.xsession

xmodmap "$HOME/.xmodmaprc"
unclutter -idle 2 &
setxkbmap -option ctrl:nocaps &
display -window root -backdrop ~/pictures/wallpaper &

mpd &
clipit &
conky &
dropbox start &

exec fluxbox

