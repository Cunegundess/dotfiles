#!/usr/bin/env bash
# kitty-session-manager — unified session picker for kitty
# One keybind to rule them all: open, switch, create, close sessions
#
# fzf keybinds:
#   Enter  → switch to / create session
#   Ctrl+x → close selected session/window
#   Esc    → cancel

KITTY_SOCKET="/tmp/kitty-${KITTY_PID}"
SESSIONS_DIR="${HOME}/.config/kitty/sessions"
KITTY="kitten @ --to $KITTY_SOCKET"

if [[ ! -S "$KITTY_SOCKET" ]]; then
  echo "Error: no kitty socket." >&2; exit 1
fi

mkdir -p "$SESSIONS_DIR"

# ──────────────────────────────────────────────────────────
# Helpers
# ──────────────────────────────────────────────────────────

create_session_file() {
  local f="${SESSIONS_DIR}/${1}.kitty-session"
  [[ -f "$f" ]] && return
  cat > "$f" <<EOF
# Session: ${1}
# Created: $(date -Iseconds)
cd ${2}
layout tall
launch zsh
EOF
}

create_ssh_session_file() {
  local f="${SESSIONS_DIR}/ssh-${1}.kitty-session"
  [[ -f "$f" ]] && return
  cat > "$f" <<EOF
# Session: ${1}
# Type: ssh
# Created: $(date -Iseconds)
layout tall
launch ssh ${2}@${3}
EOF
}

switch_to() {
  $KITTY action goto_session "$1" 2>/dev/null
}

# ──────────────────────────────────────────────────────────
# Collect data (one IPC call per source)
# ──────────────────────────────────────────────────────────

# Cache open windows ONCE
open_json=$($KITTY ls 2>/dev/null)

# 1. Open windows (exclude picker tab)
open_list=$(echo "$open_json" | jq -r '
  .[].tabs[] | . as $tab | .windows[] |
  select(.title | test("^⚡ Sessions$") | not) |
  "open\t\(.title)\t\(.cwd)\t\(.id)"
' 2>/dev/null)

# 2. Saved sessions
saved_list=""
for f in "$SESSIONS_DIR"/*.kitty-session; do
  [[ -f "$f" ]] || continue
  fname=$(basename "$f" .kitty-session)
  dir=$(grep -m1 "^cd " "$f" 2>/dev/null | cut -d' ' -f2-)
  [[ -n "$dir" ]] && saved_list+=$'\n'"saved	${fname}	${dir}	${fname}"
done

# 3. Zoxide recent dirs
recent_list=""
while IFS= read -r d; do
  [[ -d "$d" ]] || continue
  recent_list+=$'\n'"recent	$(basename "$d")	${d}	$(basename "$d")"
done < <(zoxide query -l 2>/dev/null | head -30)

# 4. SSH hosts (skip github/gitlab — git only)
ssh_list=$(awk '
  /^Host / {
    if (host != "" && user != "" && hostname != "")
      printf "ssh\t%s\t%s@%s\t%s\n", host, user, hostname, host
    host = $2; user = ""; hostname = ""
  }
  /^[[:space:]]+User / { user = $2 }
  /^[[:space:]]+HostName / { hostname = $2 }
  END {
    if (host != "" && user != "" && hostname != "")
      printf "ssh\t%s\t%s@%s\t%s\n", host, user, hostname, host
  }
' ~/.ssh/config 2>/dev/null | grep -vE "github\.com|gitlab\.com")

# Build combined list
all_items=$(printf '%s\n%s\n%s\n%s\n' \
  "$open_list" "$saved_list" "$recent_list" "$ssh_list" | \
  sed '/^[[:space:]]*$/d')

[[ -z "$all_items" ]] && { echo "No items found."; exit 1; }

# ──────────────────────────────────────────────────────────
# Close handler (for Ctrl+x binding)
# ──────────────────────────────────────────────────────────

close_selected() {
  local type="$1" name="$2" extra="$3" path="$4"
  case "$type" in
    open)
      $KITTY close-window --match "id:$extra" --ignore-no-match 2>/dev/null
      echo "Closed window: $name"
      ;;
    saved)
      $KITTY action close_session "${SESSIONS_DIR}/${name}.kitty-session" 2>/dev/null
      rm -f "${SESSIONS_DIR}/${name}.kitty-session" 2>/dev/null
      echo "Closed session: $name"
      ;;
    recent)
      $KITTY action close_session "${SESSIONS_DIR}/${name}.kitty-session" 2>/dev/null
      rm -f "${SESSIONS_DIR}/${name}.kitty-session" 2>/dev/null
      echo "Closed session: $name"
      ;;
    ssh)
      $KITTY action close_session "${SESSIONS_DIR}/ssh-${name}.kitty-session" 2>/dev/null
      rm -f "${SESSIONS_DIR}/ssh-${name}.kitty-session" 2>/dev/null
      echo "Closed SSH session: $name"
      ;;
  esac
}

# ──────────────────────────────────────────────────────────
# FZF picker
# ──────────────────────────────────────────────────────────

selected=$(echo "$all_items" | fzf \
  --border=rounded \
  --reverse \
  --height 85% \
  --prompt "  Sessions > " \
  --header "  ⌥ open │ ⬡ saved │ ⌂ recent │ ⟶ ssh │ Ctrl+x close │ ↵ type" \
  --preview '
    case "{1}" in
      open)   printf "  ◉ Open Window\n\n  Title: {2}\n  CWD: {3}" ;;
      saved)  printf "  ◈ Saved Session\n\n  Name: {2}\n  CWD: {3}" ;;
      recent) ls -la "{3}" 2>/dev/null | head -15 ;;
      ssh)    printf "  ⟶ SSH Host\n\n  {3}" ;;
      *)      printf "  ↵ Custom Path\n\n  {1}" ;;
    esac
  ' \
  --preview-window=right:40%:wrap \
  --bind "ctrl-x:execute-silent(
    close_selected \"{1}\" \"{2}\" \"{4}\" \"{3}\"
  )")

[[ -z "$selected" ]] && exit 0

# ──────────────────────────────────────────────────────────
# Parse selection
# ──────────────────────────────────────────────────────────

type=$(echo "$selected" | cut -f1)
name=$(echo "$selected" | cut -f2)
path=$(echo "$selected" | cut -f3)
extra=$(echo "$selected" | cut -f4)

# ──────────────────────────────────────────────────────────
# Dispatch
# ──────────────────────────────────────────────────────────

case "$type" in
  open)
    $KITTY focus-window --match "id:$extra" 2>/dev/null
    ;;
  saved)
    switch_to "${SESSIONS_DIR}/${name}.kitty-session"
    ;;
  recent)
    create_session_file "$(basename "$path")" "$path"
    switch_to "${SESSIONS_DIR}/$(basename "$path").kitty-session"
    ;;
  ssh)
    user=$(echo "$extra" | cut -d@ -f1)
    host_addr=$(echo "$extra" | cut -d@ -f2)
    create_ssh_session_file "$name" "$user" "$host_addr"
    switch_to "${SESSIONS_DIR}/ssh-${name}.kitty-session"
    ;;
  *)
    # Free-text: resolve path via zoxide or create dir
    input="${selected/#\~/$HOME}"
    if [[ ! -d "$input" ]]; then
      resolved=$(zoxide query "$input" 2>/dev/null || true)
      if [[ -n "$resolved" && -d "$resolved" ]]; then
        input="$resolved"
      else
        mkdir -p "$input" 2>/dev/null || { echo "Cannot create: $input" >&2; exit 1; }
      fi
    fi
    safe_name=$(basename "$input")
    create_session_file "$safe_name" "$input"
    switch_to "${SESSIONS_DIR}/${safe_name}.kitty-session"
    ;;
esac
