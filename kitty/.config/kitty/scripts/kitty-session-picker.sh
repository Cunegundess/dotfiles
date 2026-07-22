#!/usr/bin/env bash
# kitty-session-picker — list and switch between open kitty windows
# Like tmux's session picker

KITTY_SOCKET="/tmp/kitty-${KITTY_PID}"

if [[ ! -S "$KITTY_SOCKET" ]]; then
  echo "Error: no kitty socket." >&2
  exit 1
fi

KITTY="kitten @ --to $KITTY_SOCKET"

# Build list: "tab  title  cwd  window_id" (tab-separated)
list=$($KITTY ls 2>/dev/null | jq -r '
  .[].tabs[] |
  . as $tab |
  .windows[] |
  [$tab.name, .title, .cwd, (.id | tostring)] | join("\t")
' 2>/dev/null)

[[ -z "$list" ]] && { echo "No windows found." >&2; exit 1; }

# fzf: show tab/title/cwd, preview cwd, return full line
picked=$(echo "$list" | fzf \
  --border \
  --reverse \
  --height 80% \
  --prompt "Sessions > " \
  --preview 'echo "Tab: {1}\nTitle: {2}\nCWD: {3}"' \
  --with-nth 1,2,3)

[[ -z "$picked" ]] && exit 0

# Extract window_id (4th field)
window_id=$(echo "$picked" | cut -f4)
$KITTY focus-window --match "id:$window_id"
