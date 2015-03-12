# Torstein's .bashrc

##################################################################
## window
##################################################################
shopt -s checkwinsize
PROMPT_COMMAND='echo -ne "\033]0;${USER-${USERNAME}}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
if [ -e /usr/share/terminfo/x/xterm-256color ]; then
  export TERM='xterm-256color'
else
  export TERM='xterm-color'
fi

##################################################################
# prompt
##################################################################
function get_git_status() {
  local old_exit_code=$?
  local status=$(git status 2>/dev/null | head -1 | awk '{print $NF}')
  if [[ "$status" == "on" ]]; then
    echo "<tag/$(git describe --always --tag)>"
  elif [ -n "$status" ]; then
    echo "<${status}>"
  fi
  return $old_exit_code
}

PS1="\[\033[0;36m\]{\[\033[0;50m\]\w\[\033[0;36m\]} \[\033[0;35m\]\$(get_git_status) \[\033[0;32m\]what now\[\033[0;39m\]\$(if [ \$? -eq 0 ]; then echo '...'; else echo '..,'; fi) "

##################################################################
# history
##################################################################
shopt -s histappend
HISTCONTROL=ignoredups
HISTSIZE=1000000
HISTFILESIZE=1000000
HISTTIMEFORMAT="%F %H:%M:%S "
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND ;}"\
'echo "$(history 1)" >> $HOME/.bash_eternal_history'

##################################################################
# aliases
##################################################################
alias de='setxkbmap de'
alias df='df -hT'
alias e='emacsclient'
alias emacs='emacs -fn terminus-12'
alias err="egrep --color -w 'ERROR|SEVERE|WARN|Exception'"
alias eternal='cat ~/.bash_eternal_history | grep'
alias grep='grep --text --ignore-case --color --exclude-dir={.svn,.git,.hg,CVS}'
alias ga='git add'
alias gb='git branch'
alias gc='git checkout'
alias gca='git commit --amend'
alias gd='git diff'
alias gg='git log --regexp-ignore-case --grep'
alias gl='git log'
alias gpr='git pull --rebase'
alias glp='git log -p'
alias gri='git rebase -i'
alias gs='git status'
alias gsh='git show'
alias gsr='git svn rebase'
alias gbud='git branch --set-upstream-to=origin/develop develop'
alias ls='ls -lh'
alias mc="mvn clean install"
alias mcc="mvn clean compile"
alias mcp="mvn clean package"
alias mda='mvn dependency:analyze'
alias mount="mount | column -t"
alias mp="mvn package -DskipTests"
alias my-public-ip="curl -s checkip.dyndns.org | sed 's/.*<body>.*: \(.*\)<\/body>.*/\1/'"
alias no='setxkbmap no'
alias nocaps='setxkbmap -option ctrl:nocaps'
alias steam='SDL_AUDIODRIVER=alsa steam'
alias t='urxvt +sb -cr red -sl 10000 -fn xft:Terminus:pixelsize=14 -bg black -fg "#dcdccc"'
alias uprompt="unset PROMPT_COMMAND; export PS1='\u@\h \w$ '"
alias us='setxkbmap us'
alias vi=vim

##################################################################
# bash completion
##################################################################
# music -> cd music
shopt -s autocd

export FIGNORE=.svn
l="
  $HOME/src/my-little-friends/bash_completion.d/tkj
  $HOME/src/ece-scripts/etc/bash_completion.d/ece
  $HOME/src/moria/voss/etc/bash_completion.d/voss
  $HOME/src/my-little-friends/bash_completion.d/mvn

  /etc/bash_completion
  /usr/share/bash-completion/bash_completion
  /etc/bash_completion.d/maven
  /etc/bash_completion.d/subversion
  /usr/share/bash-completion/git
"
for el in $l; do
  if [ -r $el ]; then
    source $el
  fi
done

##################################################################
# java
##################################################################
export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64
export MAVEN_OPTS="-Xmx1024m -XX:MaxPermSize=256m"

##################################################################
# path
##################################################################
PATH=$HOME/src/ece-scripts/usr/bin:\
$HOME/src/moria/voss/usr/bin:\
$HOME/src/my-little-friends/bash:\
$HOME/src/my-little-friends/git:\
$HOME/src/moria/src/net:\
$HOME/src/moria/src/java:\
$HOME/src/moria/src/pictures:\
/opt/local/bin:\
$JAVA_HOME/bin:\
$PATH

export CDPATH=:$HOME/src:$HOME

##################################################################
# editor
##################################################################
export EDITOR=vim

##################################################################
# p4
##################################################################
export P4CONFIG=$HOME/.p4config

##################################################################
# language & time zone
##################################################################
# export TZ='Asia/Taipei'
export LANG=en_GB.utf8
export LC_ALL=en_GB.utf8

##################################################################
# radio
##################################################################
alias radio_absolute_radio="mplayer http://ogg2.as34763.net/vr160.ogg"
alias radio_classic_rock="mplayer http://ogg2.as34763.net/vc160.ogg"
alias radio_hinet="mplayer mms://bcr.media.hinet.net/RA00000"
alias radio_klemfm="mplayer http://mms-live.online.no/p4_klem_ogg_mq"
alias radio_metro="mplayer http://stream.21stventure.com:8100"
alias radio_p1="mplayer http://malxrstream01.nrk.no/nrk-p1-56"
alias radio_p4="mplayer http://mms-live.online.no/p4_norge_ogg_lq"
alias radio_paradise="mplayer http://stream-sd.radioparadise.com:9000/rp_96.ogg"

##################################################################
# local overrides & private bash settings
##################################################################
if [ -e ~/.bashrc.private ]; then
  source ~/.bashrc.private
fi
