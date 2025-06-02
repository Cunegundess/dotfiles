
# Inicializa o prompt
autoload -Uz promptinit
promptinit

# Configura o prompt 'adam1'
# prompt adam1

# Configurações de histórico
setopt histignorealldups sharehistory
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Usa keybindings do emacs, mesmo que o EDITOR esteja configurado para vi
bindkey -e

# Completação moderna
autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

export PATH="$PATH:/usr/local/bin:/usr/bin:/bin"

# Eza alias
alias ls="eza --icons"
alias ll="eza -lg -a --icons"
alias "ls -a"="eza -lag --icons"
alias cd="z"
alias cat="batcat --theme=gruvbox-dark"

# Autosuggestions
[ -f ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh ] && source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

# Syntax Highlighting
[ -f ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ] && source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Completions
[ -f ~/.zsh/zsh-completions/zsh-completions.zsh ] && source ~/.zsh/zsh-completions/zsh-completions.zsh

# FZF (caso esteja instalado)
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Inicializa o zoxide
eval "$(zoxide init zsh)"
# Inicializa o Starship
eval "$(starship init zsh)"
export PATH=$HOME/.local/bin:$PATH
