#!/usr/bin/env bash

# Adapted from linkarzu's kitty-zoxide-session.sh
# Select a zoxide entry and switch to an existing kitty session,
# or create it if it doesn't exist.
# Also supports SSH host entries from ~/.ssh/config.

set -uo pipefail

KITTY_SOCKET="/tmp/kitty-${KITTY_PID}"
KITTY="kitten @ --to $KITTY_SOCKET"
SESSIONS_DIR="${HOME}/.config/kitty/sessions"
SCRIPT_PATH="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)/$(basename -- "${BASH_SOURCE[0]}")"

# ─── Reload handler (called by fzf --reload) ──────────────
# Must be before require_cmd checks so fzf reload works
if [[ "${1:-}" == "--reload" ]]; then
  zoxide query -l 2>/dev/null | awk -v OFS='\t' '{
    path=$0; n=split(path, parts, "/"); base=parts[n]
    if (base == "") base=path
    printf "%s\t%s  %s\n", path, base, path
  }'
  # SSH entries
  if [[ -f "$HOME/.ssh/config" ]]; then
    awk '{
      sub(/[ \t]*#.*/, "")
      if (tolower($1) == "host") {
        for (i = 2; i <= NF; i++) {
          h = $i
          if (h ~ /^[!]/) continue
          if (h ~ /[\\*?]/) continue
          print h
        }
      }
    }' "$HOME/.ssh/config" 2>/dev/null | sort -u | while IFS= read -r host; do
      [[ -z "$host" ]] && continue
      printf "%s\tssh-%s\n" "ssh:${host}" "$host"
    done
  fi
  exit 0
fi

require_cmd() {
  local cmd="$1" hint="$2"
  if ! command -v "$cmd" >/dev/null 2>&1; then
    echo "$cmd is not installed." >&2; echo "$hint" >&2; exit 1
  fi
}

require_cmd fzf "Install: yay -S fzf or sudo dnf install fzf"
require_cmd jq "Install: yay -S jq or sudo dnf install jq"
require_cmd zoxide "Install: yay -S zoxide"

if [[ ! -S "$KITTY_SOCKET" ]]; then
  echo "No kitty socket. Ensure kitty is running with remote control." >&2
  exit 1
fi

mkdir -p "$SESSIONS_DIR"

normalize_path() {
  local p="$1"
  if command -v realpath >/dev/null 2>&1; then
    realpath "$p"
  elif command -v python3 >/dev/null 2>&1; then
    python3 -c "import os; print(os.path.realpath('$p'))"
  else
    printf "%s" "$p"
  fi
}

hash_path() {
  local p="$1"
  if command -v sha256sum >/dev/null 2>&1; then
    printf "%s" "$p" | sha256sum | awk '{print $1}'
  elif command -v shasum >/dev/null 2>&1; then
    printf "%s" "$p" | shasum -a 256 | awk '{print $1}'
  elif command -v python3 >/dev/null 2>&1; then
    python3 -c "import hashlib; print(hashlib.sha256('$p'.encode()).hexdigest())"
  else
    printf "%s" "$p" | md5sum | awk '{print $1}'
  fi
}

session_exists() {
  local name="$1"
  $KITTY ls 2>/dev/null | jq -e --arg name "$name" '
    any(.[]?.tabs[]?.windows[]?; .title == $name)
  ' >/dev/null 2>&1
}

find_session_by_path() {
  local target="$1"
  local name="" pwd="" real=""
  while IFS=$'\t' read -r name pwd; do
    [[ -z "$name" || -z "$pwd" ]] && continue
    [[ ! -d "$pwd" ]] && continue
    real="$(normalize_path "$pwd")"
    [[ "$real" == "$target" ]] && { printf "%s" "$name"; return 0; }
  done < <(
    $KITTY ls 2>/dev/null | jq -r '
      .[]?.tabs[]?.windows[]?
      | [(.title // ""), (.env.PWD // .cwd // "")]
      | @tsv
    '
  )
  return 1
}

print_menu_lines() {
  zoxide query -l 2>/dev/null | awk -v OFS='\t' '{
    path=$0
    n=split(path, parts, "/")
    base=parts[n]
    if (base == "") base=path
    printf "%s\t%s  %s\n", path, base, path
  }'
  print_ssh_menu_lines
}

print_ssh_menu_lines() {
  local root_config="$HOME/.ssh/config"
  [[ -f "$root_config" ]] || return 0

  awk '
    {
      sub(/[ \t]*#.*/, "")
      if (tolower($1) == "host") {
        for (i = 2; i <= NF; i++) {
          h = $i
          if (h ~ /^[!]/) continue
          if (h ~ /[\\*?]/) continue
          print h
        }
      }
    }
  ' "$root_config" 2>/dev/null | sort -u | while IFS= read -r host; do
    [[ -z "$host" ]] && continue
    printf "%s\tssh-%s\n" "ssh:${host}" "$host"
  done
}

focus_or_launch_dir() {
  local selected_path="$1"
  local selected_real="" base="" safe_base="" hash="" short_name="" session_name=""
  local existing_session="" session_file=""

  if [[ ! -d "$selected_path" ]]; then
    echo "Directory not found: $selected_path" >&2; exit 1
  fi

  selected_real="$(normalize_path "$selected_path")"

  # Check if already open
  existing_session="$(find_session_by_path "$selected_real" 2>/dev/null || true)"
  if [[ -n "$existing_session" ]]; then
    $KITTY ls 2>/dev/null | jq -r --arg cwd "$selected_real" '
      .[].tabs[].windows[] | select(.cwd == $cwd) | .id
    ' 2>/dev/null | head -1 | while read -r win_id; do
      $KITTY focus-window --match "id:$win_id" 2>/dev/null
    done
    zoxide add -- "$selected_real" >/dev/null 2>&1 || true
    return 0
  fi

  # Create new session
  base="$(basename "$selected_path")"
  safe_base="$(printf "%s" "$base" | tr -cs 'A-Za-z0-9._-' '_')"
  hash="$(hash_path "$selected_real")"
  hash="${hash:0:4}"
  short_name="z-${safe_base}"
  session_name="$short_name"

  if session_exists "$short_name"; then
    session_name="${short_name}-${hash}"
  fi

  session_file="${SESSIONS_DIR}/${session_name}.kitty-session"

  cat >"$session_file" <<EOF
# Session: ${session_name}
# Created: $(date -Iseconds)
layout tall
cd ${selected_real}
launch --title "${base}"
focus
focus_os_window
EOF

  $KITTY action goto_session "$session_file"
  zoxide add -- "$selected_real" >/dev/null 2>&1 || true
}

focus_or_launch_ssh() {
  local host="$1"
  local safe_host="" session_file=""

  safe_host="$(printf "%s" "$host" | tr -cs 'A-Za-z0-9._-' '_')"
  session_file="${SESSIONS_DIR}/ssh-${safe_host}.kitty-session"

  cat >"$session_file" <<EOF
# Session: ${host}
# Type: ssh
# Created: $(date -Iseconds)
layout tall
launch --title "ssh-${host}" ssh ${host}
focus
focus_os_window
EOF

  $KITTY action goto_session "$session_file"
}

# ─── Main ──────────────────────────────────────────────────

set +e
printf '\033[2J\033[H'
fzf_out="$(
  fzf --exact --ansi --height=20 --reverse \
    --header="Type to filter, enter open, esc quit" \
    --prompt="Session > " \
    --no-multi \
    --with-nth=2.. \
    --no-sort \
    --tiebreak=index \
    --expect=enter,esc \
    --bind 'enter:accept' \
    --bind 'esc:abort' \
    --bind "start:reload:${SCRIPT_PATH} --reload \"{q}\"" \
    --bind "change:reload:${SCRIPT_PATH} --reload \"{q}\""
)"
fzf_rc=$?
set -e

if [[ $fzf_rc -ne 0 && -z "${fzf_out:-}" ]]; then exit 0; fi

key="$(printf "%s\n" "$fzf_out" | head -n1)"
[[ "$key" == "esc" ]] && exit 0

sel="$(printf "%s\n" "$fzf_out" | sed -n '2p' || true)"
selected_path=""
[[ -n "${sel:-}" ]] && selected_path="$(printf "%s" "$sel" | awk -F'\t' '{print $1}')"

[[ -z "${selected_path:-}" ]] && exit 0

if [[ "$selected_path" == ssh:* ]]; then
  focus_or_launch_ssh "${selected_path#ssh:}"
else
  focus_or_launch_dir "$selected_path"
fi
