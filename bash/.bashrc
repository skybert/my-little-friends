# Torstein's .bashrc

##################################################################
## window
##################################################################
shopt -s checkwinsize
PROMPT_COMMAND='echo -ne "\033]0;${USER-${USERNAME}}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
if [ -e /lib/terminfo/x/xterm-256color ]; then
  export TERM='xterm-256color'
else
  export TERM='xterm'
fi

##################################################################
# prompt
##################################################################
function get_vc_status() {
  local old_exit_code=$?
  local status=$(git status 2>/dev/null | head -1 | awk '{print $NF}')
  if [[ "$status" == "on" ]]; then
    echo "tag/$(git describe --always --tag) "
  elif [ -n "$status" ]; then
    echo "${status} "
  fi
  return $old_exit_code
}

function get_short_dir() {
  local dir=$(pwd)
  dir=${dir#/home/${USER}}
  if [ -z "${dir}" ]; then
      dir="~"
  fi
  ## TODO
  echo $dir
}

PS1="\[\033[0;35m\]\$(get_vc_status)\[\033[0;39m\]\W \$(if [ \$? -ne 0 ]; then echo '↓ '; fi)\[\033[0;32m\]$\[\033[0;39m\] "

# improved bash -x (must be exported)
export PS4='# ${BASH_SOURCE}:${LINENO}: ${FUNCNAME[0]}() - [${SHLVL},${BASH_SUBSHELL},$?] '

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
alias ......='cd ../../../../..'
alias .....='cd ../../../..'
alias ....='cd ../../..'
alias ...='cd ../..'
alias ..='cd ..'
alias al="tail -f /opt/tomcat-engine1/logs/access.$(date --iso).log | egrep -v '(indexer|lock|changelog)'"
alias cal='LC_ALL=zh_TW.UTF-8 cal'
alias cd='cd -P'
alias clipit='clipit &> /dev/null'
alias de='setxkbmap de'
alias df='df -hT -x tmpfs -x devtmpfs'
alias diff=colordiff
alias e='emacsclient --alternate-editor="" --no-wait'
alias el='tail -f /var/log/escenic/engine1{.out,-messages,-catalina.out,-tomcat}'
alias emacs='emacs -fn terminus-bold-14'
alias err="egrep --color -w 'ERROR|SEVERE|WARN|Exception'"
alias eternal='cat ~/.bash_eternal_history | grep'
alias fjm='find-jar ~/.m2/repository'
alias fjt='find-jar /opt/tomcat-engine1'
alias ga='git add'
alias gb='git branch'
alias gbud='git branch --set-upstream-to=origin/develop develop'
alias gc='git checkout'
alias gca='git commit --amend'
alias gcaf='git commit -a --fixup'
alias gcp='git cherry-pick'
alias gd='git diff --word-diff=color'
alias gdn='git --no-pager diff'
alias gg='git log --regexp-ignore-case --grep'
alias ggg='git log --patch -G'
alias gl='git log --decorate --graph'
alias gln='git --no-pager log'
alias glp='git log --decorate --patch --word-diff=color'
alias glpn='git --no-pager log -p'
alias gpr='git pull --rebase'
alias grep='grep --text --ignore-case --color --exclude-dir={.svn,.git,.hg,CVS}'
alias gri='git rebase -i'
alias gria='git rebase --interactive --autosquash'
alias gs='git status'
alias gsh='git show'
alias gsr='git svn rebase'
alias less='less -Ri'
alias ls='ls -ltrh --color'
alias mcc="mvn clean compile"
alias mci="mvn clean install --offline --fail-at-end -Dmaven.test.skip=true"
alias mcit="mvn clean install --fail-at-end"
alias mcp="mvn clean package -Dmaven.test.skip=true --fail-at-end"
alias mda='mvn dependency:analyze'
alias mdt='mvn dependency:tree | less'
alias mount="mount | column -t"
alias mp="mvn package -DskipTests"
alias no='setxkbmap no'
alias nocaps='setxkbmap -option ctrl:nocaps'
alias steam='SDL_AUDIODRIVER=alsa steam'
alias t='urxvt +sb -cr red -sl 10000 -fn xft:Terminus:pixelsize=14 -bg black -fg "#dcdccc"'
alias tb='urxvt +sb -cr red -sl 10000 -fn 10x20'
alias ts='tkj status'
alias us='setxkbmap us'
alias vi=vim
alias what-is-my-public-ip="curl -s checkip.dyndns.org | sed 's/.*<body>.*: \(.*\)<\/body>.*/\1/'"
alias x='xml_pp'

# If we're on OpenBSD, redefine some aliases
if [[ $(uname -s) == OpenBSD ]]; then
  alias ls='gls -ltrh --color'
  alias grep='grep --text --ignore-case'
fi
##################################################################
## Command for switchong to a simple prompt I always use this in Emacs
## shells, so I set the EDITOR variable to emacsclient as well.
##################################################################
function uprompt() {
  export EDITOR=emacsclient
  PROMPT_COMMAND='echo "$(history 1)" >> $HOME/.bash_eternal_history'
  export PS1='\$ '
}

##################################################################
# bash completion
##################################################################
# music → cd music
shopt -s autocd
# ls src/**/pom.xml
shopt -s globstar

export FIGNORE=.svn
l="
  $HOME/src/my-little-friends/bash_completion.d/tkj
  $HOME/src/ece-scripts/etc/bash_completion.d/ece
  $HOME/src/moria/voss/etc/bash_completion.d/p4
  $HOME/src/my-little-friends/bash_completion.d/mvn
  $HOME/src/moria/src/ecews/bin/ecews.bash_completion

  /etc/bash_completion
  /usr/share/bash-completion/bash_completion
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
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64
export MAVEN_OPTS="-Xmx1024m"

##################################################################
# man pages from non packages
##################################################################
export MANPATH=/usr/local/man/man1:$MANPATH

##################################################################
# path
##################################################################
PATH=\
$HOME/bin:\
$HOME/.cargo/bin:\
$HOME/src/ece-scripts/usr/bin:\
$HOME/src/moria/voss/usr/bin:\
$HOME/src/my-little-friends/bash:\
$HOME/src/my-little-friends/bash/vcs:\
$HOME/src/my-little-friends/git:\
$HOME/src/moria/src/net:\
$HOME/src/moria/src/java:\
$HOME/src/moria/src/graphics:\
$HOME/src/moria/src/pictures:\
/opt/local/bin:\
$JAVA_HOME/bin:\
$PATH

export CDPATH=:$HOME/src:$HOME/doc:$HOME

##################################################################
# editor
##################################################################
export EDITOR=vim

##################################################################
# language & time zone
##################################################################
# export TZ='Asia/Taipei'
export LANG=en_GB.UTF-8
export LC_ALL=en_GB.UTF-8

##################################################################
# shell check
##################################################################
export SHELLCHECK_OPTS='--shell=bash'

##################################################################
# local overrides & private bash settings
##################################################################
if [ -e ~/.bashrc.private ]; then
  source ~/.bashrc.private
fi
