
autoload -Uz compinit
# more advanced completions, already default
# fpath=(/usr/share/zsh/site-functions $fpath)
# more completions, already default
# fpath=(/usr/share/zsh/functions/Completion/Linux $fpath)
# add cargo (rust) support
fpath=(~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/zsh/site-functions $fpath)
# put other completions there, e.g. rustup
fpath=(~/.zfunc $fpath)

# completions
# autoload -Uz compinit
compinit
setopt COMPLETE_ALIASES

# history
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history
setopt HIST_IGNORE_SPACE

# prompt
# autoload -Uz promptinit
# promptinit
# prompt redhat
# almost redhat, but space at the front
# this improves to find it again.
PS1=' [%n@%m %1~]%(#.#.$) '
PS2="> "

prompt_opts=( cr percent )

# PATH variable
typeset -U path
path=(~/bin ~/.local/bin $path[@])

# 'Go faster' power user options from guide
setopt AUTO_CD

# better word separators, default is
# WORDCHARS='*?_-.[]~=/&;!#$%^(){}<>'
#export WORDCHARS='*?_[]~=&;!#$%^(){}<>'
export WORDCHARS='*?[]~=&;!#$%^(){}<>'

# aliases from bash

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# tackle typos
alias cd..="cd .."

# use kitty kittens, i.e. extensions
alias icat="kitty +kitten icat"

# setup for emacsclient
export ALTERNATE_EDITOR=""
export EDITOR=emacsclient

# fuzzy find
source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/completion.zsh

export FZF_DEFAULT_COMMAND="fd --type=f --no-ignore-vcs --hidden --exclude=.git/"

# autocompletion like fish
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
