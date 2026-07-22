#!/usr/bin/env bash
# kitty-session-manager — fzf session picker with zoxide + SSH support
# Opens in a temporary kitty tab via Alt+s, closes on exit.

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
  name=$(basename "$f" .kitty-session)
  dir=$(grep -m1 "^cd " "$f" 2>/dev/null | cut -d' ' -f2-)
  [[ -n "$dir" ]] && saved_sessions+=$'\n'"saved	${name}	${dir}	"
done

# 3. Zoxide recent directories (skip dirs already in saved sessions)
recent_dirs=""
while IFS= read -r d; do
  skip=false
  for f in "$SESSIONS_DIR"/*.kitty-session; do
    [[ -f "$f" ]] || continue
    sdir=$(grep -m1 "^cd " "$f" 2>/dev/null | cut -d' ' -f2-)
    [[ "$d" == "$sdir" ]] && skip=true && break
  done
  $skip && continue
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
  --header "  open: windows | saved: sessions | recent: zoxide | ssh: remote" \
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
  session_file="${SESSIONS_DIR}/${safe_name}.kitty-session"
  cat > "$session_file" <<EOF
# Session: ${safe_name}
# Created: $(date -Iseconds)
cd ${input}
layout tall
EOF
  $KITTY launch --type=tab --cwd="$input" --tab-title="$safe_name" zsh 2>/dev/null
  exit 0
fi

# --- Known categories ---
case "$type" in
  open)
    $KITTY focus-window --match "id:$extra" 2>/dev/null
    ;;
  saved)
    title=$(basename "$path")
    $KITTY launch --type=tab --cwd="$path" --tab-title="$title" zsh 2>/dev/null
    ;;
  recent)
    safe_name=$(basename "$path")
    session_file="${SESSIONS_DIR}/${safe_name}.kitty-session"
    cat > "$session_file" <<EOF
# Session: ${safe_name}
# Created: $(date -Iseconds)
cd ${path}
layout tall
EOF
    $KITTY launch --type=tab --cwd="$path" --tab-title="$safe_name" zsh 2>/dev/null
    ;;
  ssh)
    user=$(echo "$extra" | cut -d@ -f1)
    host_addr=$(echo "$extra" | cut -d@ -f2)
    session_file="${SESSIONS_DIR}/ssh-${name}.kitty-session"
    cat > "$session_file" <<EOF
# Session: ${name}
# Type: ssh
# Created: $(date -Iseconds)
layout tall
launch ssh ${user}@${host_addr}
EOF
    $KITTY launch --type=tab --tab-title="$name" ssh "$user@$host_addr" 2>/dev/null
    ;;
esac
