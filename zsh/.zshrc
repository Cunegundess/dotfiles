### ┌───────────────────────────────────────────────┐
### │            Inicialização do Zsh              │
### └───────────────────────────────────────────────┘

if command -v tmux &> /dev/null && [ -z "$TMUX" ]; then
  tmux attach-session -t main || tmux new-session -s main
fi

autoload -Uz promptinit; promptinit
autoload -Uz compinit; compinit

# Carregamento de plugins do Zinit
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
  print -P "%F{33} %F{220}Installing Zinit (%F{33}zdharma-continuum/zinit%F{220})…%f"
  command mkdir -p "$HOME/.local/share/zinit"
  command chmod g-rwX "$HOME/.local/share/zinit"
  command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
    print -P "%F{33} %F{34}Installation successful.%f%b" || \
    print -P "%F{160} The clone has failed.%f%b"
fi

source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

### ┌───────────────────────────────────────────────┐
### │              Plugins com Zinit               │
### └───────────────────────────────────────────────┘

# Annexes
zinit light-mode for \
  zdharma-continuum/zinit-annex-as-monitor \
  zdharma-continuum/zinit-annex-bin-gem-node \
  zdharma-continuum/zinit-annex-patch-dl \
  zdharma-continuum/zinit-annex-rust

# Plugins principais
zinit light zsh-users/zsh-autosuggestions
zinit light zsh-users/zsh-completions
zinit light zsh-users/zsh-syntax-highlighting
zinit light Aloxaf/fzf-tab
zinit light MichaelAquilina/zsh-auto-notify

### ┌───────────────────────────────────────────────┐
### │           Comportamento e Estilo             │
### └───────────────────────────────────────────────┘

bindkey -e
fastfetch

# Editor padrão
export EDITOR="nvim"
export VISUAL="nvim"
export SUDO_EDITOR="nvim"

### ┌───────────────────────────────────────────────┐
### │             Histórico do shell               │
### └───────────────────────────────────────────────┘

setopt histignorealldups sharehistory
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

### ┌───────────────────────────────────────────────┐
### │        Configurações de completação           │
### └───────────────────────────────────────────────┘

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

### ┌───────────────────────────────────────────────┐
### │              Alias e atalhos                 │
### └───────────────────────────────────────────────┘

alias wifi="nmtui"

# Substituições de comandos
alias ls="eza --icons"
alias ll="eza -lg -a --icons"
alias "ls -a"="eza -lag --icons"
alias cd="z"
alias cat="batcat --theme=ansi"

# IP local
if [[ -x "$(command -v ip)" ]]; then
  alias iplocal="ip -br -c a"
else
  alias iplocal="ifconfig | grep -Eo 'inet (addr:)?([0-9]*\.){3}[0-9]*' | grep -Eo '([0-9]*\.){3}[0-9]*' | grep -v '127.0.0.1'"
fi

# IP externo
if [[ -x "$(command -v curl)" ]]; then
  alias ipexternal="curl -s ifconfig.me && echo"
elif [[ -x "$(command -v wget)" ]]; then
  alias ipexternal="wget -qO- ifconfig.me && echo"
fi

### ┌───────────────────────────────────────────────┐
### │               Ferramentas extras              │
### └───────────────────────────────────────────────┘

export PATH="$HOME/.config/emacs/bin:$PATH"
export PATH="$HOME/.config/emacs/bin/doom:$PATH"

# FZF (se instalado)
# [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# FZF keybindings e completions
[[ -f /usr/share/doc/fzf/examples/key-bindings.zsh ]] && source /usr/share/doc/fzf/examples/key-bindings.zsh
[[ -f /usr/share/doc/fzf/examples/completion.zsh ]] && source /usr/share/doc/fzf/examples/completion.zsh

# Zoxide
eval "$(zoxide init zsh)"

# Starship prompt
eval "$(starship init zsh)"

# Comando wrapper para yazi (alternativa ao ranger)
function y() {
  local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
  yazi "$@" --cwd-file="$tmp"
  IFS= read -r -d '' cwd < "$tmp"
  [ -n "$cwd" ] && [ "$cwd" != "$PWD" ] && builtin cd -- "$cwd"
  rm -f -- "$tmp"
}

# eval "$(ssh-agent -s)"
# ssh-add $HOME/.ssh/github
# ssh-add $HOME/.ssh/id_ed25519

### ┌───────────────────────────────────────────────┐
### │                  PATHs                       │
### └───────────────────────────────────────────────┘

export PATH="$HOME/.local/bin:/usr/local/bin:/usr/bin:/bin:$PATH"

# NVM (Node Version Manager)
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && source "$NVM_DIR/bash_completion"

