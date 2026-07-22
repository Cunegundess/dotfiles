#!/usr/bin/env bash
# kitty-session-manager — unified fzf picker for sessions
# Creates, switches, and focuses — all in one picker via Alt+s

KITTY_SOCKET="/tmp/kitty-${KITTY_PID}"
SESSIONS_DIR="${HOME}/.config/kitty/sessions"

if [[ ! -S "$KITTY_SOCKET" ]]; then
  echo "Error: no kitty socket. Ensure kitty is running with:" >&2
  echo "  allow_remote_control socket-only" >&2
  echo "  listen_on unix:/tmp/kitty-{kitty_pid}" >&2
  exit 1
fi

KITTY="kitten @ --to $KITTY_SOCKET"
mkdir -p "$SESSIONS_DIR"

# --- Helpers ---

# Find a session file by directory path
find_session_by_dir() {
  local target_dir="$1"
  for f in "$SESSIONS_DIR"/*.kitty-session; do
    [[ -f "$f" ]] || continue
    local sdir
    sdir=$(grep -m1 "^cd " "$f" 2>/dev/null | cut -d' ' -f2-)
    # Expand ~ in session file to match
    local expanded_sdir="${sdir/#\~/$HOME}"
    if [[ "$target_dir" == "$expanded_sdir" || "$target_dir" == "$sdir" ]]; then
      echo "$f"
      return 0
    fi
  done
  return 1
}

# Find a session file by SSH host name
find_session_by_ssh() {
  local host_name="$1"
  local candidate="${SESSIONS_DIR}/ssh-${host_name}.kitty-session"
  [[ -f "$candidate" ]] && echo "$candidate" && return 0
  return 1
}

# Check if a window is open at a given CWD, return window id
find_window_by_cwd() {
  local target_cwd="$1"
  $KITTY ls 2>/dev/null | jq -r --arg cwd "$target_cwd" '
    .[].tabs[].windows[] |
    select(.cwd == $cwd or (.cwd + "/" | startswith($cwd + "/")) or ($cwd + "/" | startswith(.cwd + "/"))) |
    .id
  ' 2>/dev/null | head -1
}

# Focus a window by id, returns 0 if success
focus_window() {
  local win_id="$1"
  [[ -n "$win_id" ]] && $KITTY focus-window --match "id:$win_id" 2>/dev/null && return 0
  return 1
}

# Open a saved session via goto_session (kitty native)
open_session() {
  local session_file="$1"
  $KITTY action goto_session "$session_file" 2>/dev/null
}

# Create session file for a local directory
create_session_file() {
  local session_name="$1"
  local dir="$2"
  local session_file="${SESSIONS_DIR}/${session_name}.kitty-session"
  cat > "$session_file" <<EOF
# Session: ${session_name}
# Created: $(date -Iseconds)
new_tab ${session_name}
cd ${dir}
layout tall
launch zsh
EOF
  echo "$session_file"
}

# Create session file for SSH
create_ssh_session_file() {
  local host_name="$1"
  local user="$2"
  local addr="$3"
  local session_file="${SESSIONS_DIR}/ssh-${host_name}.kitty-session"
  cat > "$session_file" <<EOF
# Session: ${host_name}
# Type: ssh
# Created: $(date -Iseconds)
new_tab ${host_name}
layout tall
launch ssh ${user}@${addr}
EOF
  echo "$session_file"
}

# Smart action: if window open at dir → focus, else if session → load, else create+load
smart_open_dir() {
  local dir="$1"
  local name="$2"

  # 1. Check if already open
  local win_id
  win_id=$(find_window_by_cwd "$dir")
  if [[ -n "$win_id" ]]; then
    focus_window "$win_id"
    return 0
  fi

  # 2. Check if session file exists
  local session_file
  session_file=$(find_session_by_dir "$dir" 2>/dev/null)
  if [[ -n "$session_file" ]]; then
    open_session "$session_file"
    return 0
  fi

  # 3. Create new session + open
  session_file=$(create_session_file "$name" "$dir")
  open_session "$session_file"
}

# --- Collect data ---

# 1. Open windows
open_windows=$($KITTY ls 2>/dev/null | jq -r '
  .[].tabs[] | . as $tab | .windows[] |
  "open\t\(.title)\t\(.cwd)\t\(.id)"
' 2>/dev/null)

# 2. Saved sessions
saved_sessions=""
for f in "$SESSIONS_DIR"/*.kitty-session; do
  [[ -f "$f" ]] || continue
  fname=$(basename "$f" .kitty-session)
  dir=$(grep -m1 "^cd " "$f" 2>/dev/null | cut -d' ' -f2-)
  [[ -n "$dir" ]] && saved_sessions+=$'\n'"saved	${fname}	${dir}	"
done

# 3. Zoxide recent directories (skip dirs that already have a session)
recent_dirs=""
while IFS= read -r d; do
  [[ -d "$d" ]] || continue
  # Skip if already in saved sessions
  find_session_by_dir "$d" >/dev/null 2>&1 && continue
  # Skip if already in open windows
  find_window_by_cwd "$d" >/dev/null 2>&1 && continue
  recent_dirs+=$'\n'"recent	$(basename "$d")	${d}	"
done < <(zoxide query -l 2>/dev/null | head -20)

# 4. SSH hosts from config (skip github/gitlab — git-only)
ssh_hosts=$(awk '
  /^Host / {
    if (host != "" && user != "" && hostname != "")
      printf "ssh\t%s\t%s@%s\t\n", host, user, hostname
    host = $2; user = ""; hostname = ""
  }
  /^[[:space:]]+User / { user = $2 }
  /^[[:space:]]+HostName / { hostname = $2 }
  END {
    if (host != "" && user != "" && hostname != "")
      printf "ssh\t%s\t%s@%s\t\n", host, user, hostname
  }
' ~/.ssh/config 2>/dev/null | grep -vE "github\.com|gitlab\.com")

# --- Build combined list ---
all_items=$(printf '%s\n%s\n%s\n%s\n' \
  "$open_windows" "$saved_sessions" "$recent_dirs" "$ssh_hosts" | \
  sed '/^[[:space:]]*$/d')

[[ -z "$all_items" ]] && { echo "No items found."; exit 1; }

# --- FZF picker ---
selected=$(echo "$all_items" | fzf \
  --border=rounded \
  --reverse \
  --height 85% \
  --prompt "  Sessions > " \
  --header "  open | saved | recent | ssh" \
  --preview '
    case "{1}" in
      open)   printf "  Open Window\n  Title: {2}\n  CWD: {3}" ;;
      saved)  printf "  Saved Session\n  Name: {2}\n  CWD: {3}" ;;
      recent) ls -la "{3}" 2>/dev/null | head -15 ;;
      ssh)    printf "  SSH Host\n  {3}" ;;
    esac
  ' \
  --preview-window=right:40%:wrap)

[[ -z "$selected" ]] && exit 0

# --- Parse selection ---
type=$(echo "$selected" | cut -f1)
name=$(echo "$selected" | cut -f2)
path=$(echo "$selected" | cut -f3)
extra=$(echo "$selected" | cut -f4)

# --- Free-text input (typed something not in list) ---
if [[ "$type" != "open" && "$type" != "saved" && "$type" != "recent" && "$type" != "ssh" ]]; then
  input="$selected"
  input="${input/#\~/$HOME}"

  if [[ ! -d "$input" ]]; then
    resolved=$(zoxide query "$input" 2>/dev/null || true)
    if [[ -n "$resolved" && -d "$resolved" ]]; then
      input="$resolved"
    else
      mkdir -p "$input" 2>/dev/null || { echo "Cannot create: $input" >&2; exit 1; }
    fi
  fi

  safe_name=$(basename "$input")
  smart_open_dir "$input" "$safe_name"
  exit 0
fi

# --- Known categories ---
case "$type" in
  open)
    focus_window "$extra"
    ;;
  saved)
    # Already open? Focus. Otherwise load session via goto_session.
    win_id=$(find_window_by_cwd "$path")
    if [[ -n "$win_id" ]]; then
      focus_window "$win_id"
    else
      session_file="${SESSIONS_DIR}/${name}.kitty-session"
      open_session "$session_file"
    fi
    ;;
  recent)
    smart_open_dir "$path" "$(basename "$path")"
    ;;
  ssh)
    user=$(echo "$extra" | cut -d@ -f1)
    host_addr=$(echo "$extra" | cut -d@ -f2)

    # Check if session already exists
    session_file=$(find_session_by_ssh "$name" 2>/dev/null)
    if [[ -n "$session_file" ]]; then
      open_session "$session_file"
    else
      session_file=$(create_ssh_session_file "$name" "$user" "$host_addr")
      open_session "$session_file"
    fi
    ;;
esac
