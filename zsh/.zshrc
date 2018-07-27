
# completions
autoload -Uz compinit
compinit
setopt COMPLETE_ALIASES

# prompt
autoload -Uz promptinit
promptinit
prompt redhat

# PATH variable
typeset -U path
path=(~/bin ~/.local/bin $path[@])
