# Torstein's .bashrc

# window
shopt -s checkwinsize
PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
TERM=rxvt-256color

# prompt
PS1="\[\033[0;36m\]{\[\033[0;50m\]\w\[\033[0;36m\]} \[\033[0;32m\]what now... \[\033[0;39m\]"

# history
shopt -s histappend
HISTCONTROL=ignoredups
HISTSIZE=1000
HISTFILESIZE=2000
HISTTIMEFORMAT="%F %H:%M:%S "
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND ;}"\
'echo "$(history 1)" >> ~/.bash_eternal_history'

# aliases
alias df='df -hT'
alias ls='ls -lh'
alias uprompt="unset PROMPT_COMMAND; export PS1='\u@\h \w$ '"

# bash completion
l="~/src/my-little-friends/bash_completion.d/tkj
~/src/ece-scripts/etc/bash_completion.d/ece"
for el in $el; do
  if [ -e $el ]; then
    source $el
  fi
done

# path
PATH=$HOME/src/ece-scripts/usr/bin:\
$HOME/src/my-little-friends/bash:\
/opt/maven/bin:\
$PATH

# p4
export P4CONFIG=$HOME/.p4config

# language & time zone
export TZ='Europe/Oslo'
export LANG=en_GB.utf8
export LC_ALL=en_GB.utf8

# local overrides & private bash settings
if [ -e ~/.bashrc.private ]; then
  source ~/.bashrc.private
fi

