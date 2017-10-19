# Setup fzf
# ---------
if [[ ! "$PATH" == */usr/local/src/fzf/bin* ]]; then
  export PATH="$PATH:/usr/local/src/fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/usr/local/src/fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "/usr/local/src/fzf/shell/key-bindings.bash"

# Setting ag as the default source for fzf
# and include hidden files
export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'

# To apply the command to CTRL-T as well
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"


