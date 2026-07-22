#!/usr/bin/env bash
# Kitty sessionizer — fzf-based directory navigator
# Equivalent to tmux-sessionizer.sh but using kitten @ commands
# Trigger: kitty_mod+s (via kitten sessions) or manually

find_kitty_socket() {
    # Find the kitty socket by looking for kitty processes
    local pid
    pid=$(pgrep -f "^kitty" | head -1)
    if [[ -n "$pid" ]]; then
        local socket="/tmp/kitty-${pid}"
        if [[ -S "$socket" ]]; then
            echo "$socket"
            return
        fi
    fi
    # Fallback: find any kitty socket
    local socket
    socket=$(find /tmp -maxdepth 1 -name "kitty-*" -type s 2>/dev/null | head -1)
    if [[ -n "$socket" ]]; then
        echo "$socket"
        return
    fi
    return 1
}

KITTY_SOCKET=$(find_kitty_socket)

if [[ -z "$KITTY_SOCKET" || ! -S "$KITTY_SOCKET" ]]; then
    echo "Error: Cannot find kitty socket. Make sure kitty is running with:"
    echo "  allow_remote_control socket-only"
    echo "  listen_on unix:/tmp/kitty-{kitty_pid}"
    exit 1
fi

KITTY="kitten @ --to $KITTY_SOCKET"

select_directory() {
    local selected
    selected=$(
        find "$HOME" -mindepth 1 -maxdepth 3 -type d \
            -not -path "*/\.*" \
            -not -path "*/node_modules/*" \
            -not -path "*/.git/*" \
            -not -path "*/vendor/*" \
            -not -path "*/__pycache__/*" \
            2>/dev/null | \
        fzf --border --print-query --preview 'ls -la {}' --reverse --height 80%
    )
    echo "$selected" | tail -1
}

clone_repo() {
    local url="$1"
    local repo_name target_dir

    repo_name=$(basename "$url" .git)

    if [[ -n "$DEV_DIR" ]]; then
        target_dir="${DEV_DIR}/${repo_name}"
    else
        target_dir="$HOME/projects/${repo_name}"
    fi

    echo "Cloning '$repo_name' to '$target_dir'..."

    if [[ -d "$target_dir" ]]; then
        echo "Directory already exists: $target_dir"
        echo "$target_dir"
        return
    fi

    mkdir -p "$(dirname "$target_dir")"

    if git clone "$url" "$target_dir" 2>/dev/null; then
        echo "Repository cloned successfully to $target_dir"
        echo "$target_dir"
    else
        echo "Failed to clone repository"
        return 1
    fi
}

main() {
    local selected

    if [[ $# -eq 1 ]]; then
        selected="$1"
    else
        selected=$(select_directory)
    fi

    if [[ -z "$selected" ]]; then
        exit 0
    fi

    # Handle GitHub/GitLab URLs
    if [[ "$selected" == https://github.com/* || "$selected" == git@github.com:* || \
          "$selected" == https://gitlab.com/* || "$selected" == git@gitlab.com:* ]]; then
        selected=$(clone_repo "$selected")
        if [[ $? -ne 0 || -z "$selected" ]]; then
            exit 1
        fi
    fi

    # Handle non-existent directories
    if [[ ! -d "$selected" ]]; then
        read -p "Directory '$selected' doesn't exist. Create it? (y/n) " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            mkdir -p "$selected"
        else
            exit 0
        fi
    fi

    # Check if a kitty window is already open at this directory
    local existing_windows
    existing_windows=$($KITTY ls 2>/dev/null | grep -c "$selected" || true)

    if [[ "$existing_windows" -gt 0 ]]; then
        # Focus the existing window
        local window_id
        window_id=$($KITTY ls 2>/dev/null | grep "$selected" | head -1 | awk '{print $1}')
        if [[ -n "$window_id" ]]; then
            $KITTY focus-window --match "id:$window_id"
            echo "Focused existing window for: $selected"
            exit 0
        fi
    fi

    # Create a new window in the selected directory
    $KITTY launch --type=tab --cwd="$selected" zsh
    echo "Opened: $selected"
}

main "$@"
