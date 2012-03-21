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

# exec ck-launch-session gnome-session
# ck-launch-session is done by the login manager, see ps auxww.
exec fluxbox

