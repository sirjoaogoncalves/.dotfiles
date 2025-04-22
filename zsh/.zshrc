# Path to your oh-my-zsh installation (if you're using it)
export ZSH="$HOME/.oh-my-zsh"

# Set theme (a simple, informative theme that shows git status)
ZSH_THEME="af-magic"

# History configuration
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history
setopt HIST_IGNORE_ALL_DUPS  # Don't record duplicates
setopt HIST_SAVE_NO_DUPS     # Don't save duplicates
setopt HIST_FIND_NO_DUPS     # When searching history, don't show duplicates
setopt SHARE_HISTORY         # Share history between sessions
setopt EXTENDED_HISTORY      # Add timestamps to history

# Useful productivity plugins (no autocompletion)
plugins=(
  git                # Git aliases and functions
  docker             # Docker aliases
  extract            # Extract various archive formats with a single command
  z                  # Jump to frequently accessed directories
  fzf                # Fuzzy finder integration
  ssh-agent          # Automatically start ssh-agent
  zsh-syntax-highlighting  # Syntax highlighting in the terminal
)

# Initialize oh-my-zsh
source $ZSH/oh-my-zsh.sh

# User configuration
export EDITOR='nvim'  # Set default editor to neovim
export VISUAL='nvim'

# Fast directory navigation with z
[ -f ~/.z.sh ] && source ~/.z.sh

# Aliases
alias vim='nvim'
alias vi='nvim'
alias zshconfig="nvim ~/.zshrc"
alias ohmyzsh="nvim ~/.oh-my-zsh"

# Git aliases
alias gs='git status'
alias ga='git add'
alias gc='git commit'
alias gp='git push'
alias gl='git pull'
alias gd='git diff'
alias gb='git branch'
alias gco='git checkout'
alias gl='git log --oneline --graph --decorate'

# Docker aliases
alias dps='docker ps'
alias dpsa='docker ps -a'
alias di='docker images'
alias dexec='docker exec -it'
alias dlogs='docker logs'

# System aliases
alias ll='ls -la'
alias la='ls -A'
alias l='ls -CF'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

# Utility functions
# Open files with fzf
vf() {
  local file
  file=$(fzf --preview 'bat --style=numbers --color=always {}' --height 50% --layout reverse)
  if [ -n "$file" ]; then
    nvim "$file"
  fi
}

# Find and cd to directory using fzf
fd() {
  local dir
  dir=$(find ${1:-.} -type d -not -path "*/\.*" | fzf +m) && cd "$dir"
}

# Search history with fzf
fh() {
  print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed -E 's/ *[0-9]*\*? *//' | sed -E 's/\\/\\\\/g')
}

# Set up key bindings for fzf if installed
if [ -f ~/.fzf.zsh ]; then
  source ~/.fzf.zsh
fi

# If you use direnv for per-directory environment variables
if command -v direnv > /dev/null; then
  eval "$(direnv hook zsh)"
fi

# Add your custom paths
export PATH="$HOME/bin:$HOME/.local/bin:$PATH"

# Prevent nested ranger instances
function ranger() {
    if [ -z "$RANGER_LEVEL" ]; then
        /usr/bin/ranger "$@"
    else
        exit
    fi
}

# Make directory and cd into it
function mkcd() {
    mkdir -p "$1" && cd "$1"
}

# Custom key bindings
bindkey '^[[A' history-beginning-search-backward
bindkey '^[[B' history-beginning-search-forward

# If you use SSH frequently, enable SSH agent management
zstyle :omz:plugins:ssh-agent agent-forwarding on
