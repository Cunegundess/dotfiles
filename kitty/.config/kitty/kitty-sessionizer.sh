#!/usr/bin/env bash
# Kitty sessionizer — fzf-based directory navigator
# Equivalent to tmux-sessionizer.sh but using kitten @ commands

KITTY_SOCKET="/tmp/kitty-${KITTY_PID}"

if [[ ! -S "$KITTY_SOCKET" ]]; then
  echo "Error: no kitty socket. Ensure kitty is running with:" >&2
  echo "  allow_remote_control socket-only" >&2
  echo "  listen_on unix:/tmp/kitty-{kitty_pid}" >&2
  exit 1
fi

KITTY="kitten @ --to $KITTY_SOCKET"

select_directory() {
  find "$HOME" -mindepth 1 -maxdepth 3 -type d \
    -not -path "*/\.*" \
    -not -path "*/node_modules/*" \
    -not -path "*/.git/*" \
    -not -path "*/vendor/*" \
    -not -path "*/__pycache__/*" \
    2>/dev/null | \
  fzf --border --preview 'ls -la {}' --reverse --height 80%
}

clone_repo() {
  local url="$1"
  local repo_name target_dir

  repo_name=$(basename "$url" .git)
  target_dir="${DEV_DIR:-$HOME/projects}/${repo_name}"

  if [[ -d "$target_dir" ]]; then
    echo "$target_dir"
    return
  fi

  mkdir -p "$(dirname "$target_dir")"
  if git clone "$url" "$target_dir" 2>/dev/null; then
    echo "$target_dir"
  else
    echo "Failed to clone $url" >&2
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

  [[ -z "$selected" ]] && exit 0

  # Handle GitHub/GitLab URLs
  if [[ "$selected" =~ ^(https://github\.com/|git@github\.com:|https://gitlab\.com/|git@gitlab\.com:) ]]; then
    selected=$(clone_repo "$selected") || exit 1
    [[ -z "$selected" ]] && exit 0
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
  local existing_window
  existing_window=$($KITTY ls 2>/dev/null | python3 -c "
import sys, json
data = json.load(sys.stdin)
target = '$selected'
for os_win in data:
    for tab in os_win['tabs']:
        for win in tab['windows']:
            cwd = win.get('cwd', '')
            if cwd == target or cwd.startswith(target + '/'):
                print(win['id'])
                sys.exit(0)
" 2>/dev/null)

  if [[ -n "$existing_window" ]]; then
    $KITTY focus-window --match "id:$existing_window"
    exit 0
  fi

  # Open new window in the selected directory
  local title
  title=$(basename "$selected")
  $KITTY launch --type=window --cwd="$selected" --title="$title" zsh
}

main "$@"
