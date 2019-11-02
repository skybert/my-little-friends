# Moved →
This repo has moved to https://gitlab.com/skybert/my-little-friends/


The wee scripts and configuration files I cannot live without.


## These are the commands I use to set up a new computer

```
mkdir src && cd src
git clone git@github.com:skybert/skybert-net.git
cd ~/

# bash
ln -s src/my-little-friends/bash/.bashrc 
ln -s src/my-little-friends/bash/.inputrc
ln -s src/my-little-friends/bash/.fzf.bash

# emacs
ln -s src/my-little-friends/emacs/.emacs
ln -s src/my-little-friends/emacs/.emacs.d

# x
ln -s src/my-little-friends/x/.xsession
ln -s src/my-little-friends/x/.xmodmaprc
ln -s src/my-little-friends/x/x-resources ~/.Xresources

# xdg
ln -s ~/src/my-little-friends/xdg/.config/user-dirs.dirs ~/.config/user-dirs.dirs
ln -s ~/src/my-little-friends/xdg/.config/user-dirs.locale ~/.config/user-dirs.locale

# git
ln -s src/my-little-friends/git/.gitignore
ln -s src/my-little-friends/git/.gitconfig-work  ~/.gitconfig 

# top
ln -s src/my-little-friends/top/.toprc

# vim
ln -s src/my-little-friends/vim/.vimrc

# mpd
ln -s src/my-little-friends/mpd/mpd.conf .mpdconf

# urxvt
ln -s src/my-little-friends/x/.urxvt 

# i3
ln -s src/my-little-friends/i3/config ~/.config/i3

```
