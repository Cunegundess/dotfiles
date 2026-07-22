#!/usr/bin/env bash

# Adapted from linkarzu's kitty-zoxide-session.sh
# Select a zoxide entry and switch to an existing kitty session,
# or create it if it doesn't exist.
# Also supports SSH host entries from ~/.ssh/config.

set -uo pipefail

KITTY_SOCKET_PATH="/tmp/kitty-${KITTY_PID}"
KITTY_SOCKET="unix:${KITTY_SOCKET_PATH}"
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

if [[ ! -S "$KITTY_SOCKET_PATH" ]]; then
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

# ─── Directory sessions ─────────────────────────────────────
#
# NOTE: we deliberately do NOT try to detect "is this session already
# open" by comparing kitty window cwd's. That approach is fragile:
# it breaks the moment the user `cd`s inside the shell, and kitty
# already solves this problem natively via `goto_session`, which
# tracks sessions by their file path and switches to them if already
# loaded, or loads them fresh if not.
#
# So: compute a deterministic session file name for the directory
# (keyed off the FULL real path, not just basename, to avoid
# collisions between differently-located dirs with the same name),
# write it once if missing, and always hand off to goto_session.
focus_or_launch_dir() {
  local selected_path="$1"
  local selected_real="" base="" safe_base="" hash="" session_name="" session_file=""

  if [[ ! -d "$selected_path" ]]; then
    echo "Directory not found: $selected_path" >&2; exit 1
  fi

  selected_real="$(normalize_path "$selected_path")"

  base="$(basename "$selected_path")"
  safe_base="$(printf "%s" "$base" | tr -cs 'A-Za-z0-9._-' '_')"
  hash="$(hash_path "$selected_real")"
  hash="${hash:0:8}"
  session_name="z-${safe_base}-${hash}"
  session_file="${SESSIONS_DIR}/${session_name}.kitty-session"

  if [[ ! -f "$session_file" ]]; then
    cat >"$session_file" <<EOF
# Session: ${session_name}
# Created: $(date -Iseconds)
layout tall
cd ${selected_real}
launch --title "${base}"
EOF
  fi

  $KITTY action goto_session "$session_file"
  zoxide add -- "$selected_real" >/dev/null 2>&1 || true
}

focus_or_launch_ssh() {
  local host="$1"
  local safe_host="" session_file=""

  safe_host="$(printf "%s" "$host" | tr -cs 'A-Za-z0-9._-' '_')"
  session_file="${SESSIONS_DIR}/ssh-${safe_host}.kitty-session"

  if [[ ! -f "$session_file" ]]; then
    cat >"$session_file" <<EOF
# Session: ${host}
# Type: ssh
# Created: $(date -Iseconds)
layout tall
launch --title "ssh-${host}" ssh ${host}
EOF
  fi

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
