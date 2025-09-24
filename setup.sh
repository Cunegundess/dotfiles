#!/bin/bash

REPO_PATH="$HOME/dotfiles"
BRANCH="main"
SYNC_EVERY_MINUTES=30
PROGRAMS="nvim tmux zsh fonts ghostty gtk-icons gtk-themes starship wallpapers"

sync_stow() {
    cd "$REPO_PATH" || exit 1

    if ! command -v stow &>/dev/null; then
        echo "[$(date)] Instalando stow..."

        if command -v apt &>/dev/null; then
            sudo apt update && sudo apt install -y stow
        elif command -v dnf &>/dev/null; then
            sudo dnf install -y stow
        fi

        echo "[$(date)] Stow instalado com sucesso!"
    fi

    echo "[$(date)] Aplicando stow..."
    stow -t $HOME $PROGRAMS
    echo "[$(date)] Stow completo: $PROGRAMS"
}

sync_git() {
    cd "$REPO_PATH" || exit 1

    if git diff-index --quiet HEAD --; then
        echo "[$(date)] Nada pra commitar"
        return
    fi

    git pull --rebase
    git add .
    git commit -m "Auto-sync: $(date '+%Y-%m-%d %H:%M:%S')"
    git push origin "$BRANCH"
    echo "[$(date)] Sync completo!"
}

FIRST_RUN_FILE="$REPO_PATH/.dotfiles_auto_configured"

if [ ! -f "$FIRST_RUN_FILE" ]; then
    echo "🔧 Primeira execução - Configurando auto-sync..."
    setup_cron() {
        CRON_JOB="*/$SYNC_EVERY_MINUTES * * * * $PWD/$(basename $0)"
        (crontab -l 2>/dev/null | grep -v "$(basename $0)") | crontab -
        (
            crontab -l 2>/dev/null
            echo "$CRON_JOB"
        ) | crontab -
        echo "Agendado para rodar a cada $SYNC_EVERY_MINUTES minutos"
    }

    setup_cron
    sync_stow
    touch "$FIRST_RUN_FILE"
fi

sync_git
