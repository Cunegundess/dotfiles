#!/usr/bin/env bash

# Shows open kitty windows in fzf and switches between them
# Groups by CWD (each directory = a "session")
# Vim-like modes:
# - Normal mode: j/k move, d close, enter open, i insert, esc quit
# - Insert mode: type to filter, enter open, esc normal

set -uo pipefail

KITTY_SOCKET="/tmp/kitty-${KITTY_PID}"
KITTY="kitten @ --to $KITTY_SOCKET"

if ! command -v fzf >/dev/null 2>&1; then echo "fzf not found." >&2; exit 1; fi
if ! command -v jq >/dev/null 2>&1; then echo "jq not found." >&2; exit 1; fi
if [[ ! -S "$KITTY_SOCKET" ]]; then echo "No kitty socket." >&2; exit 1; fi

build_menu_lines() {
  local sessions_tsv=""
  sessions_tsv="$(
    $KITTY ls 2>/dev/null | jq -r '
      [
        .[] as $os
        | $os.tabs[] as $tab
        | $tab.windows[]?
        | {
            cwd: (.env.PWD // .cwd),
            title: .title,
            os_focused: ($os.is_focused // false),
            tab_focused: ($tab.is_focused // false),
            last_focused_at: (.last_focused_at // 0),
            win_id: .id
          }
      ]
      | sort_by(-.last_focused_at)
      | group_by(.cwd)
      | map({
          cwd_last_focused_at: (map(.last_focused_at) | max),
          pick: (
            if (map(.os_focused and .tab_focused) | any) then
              (map(select(.os_focused and .tab_focused)) | .[0])
            else
              .[0]
            end
          )
        })
      | map(.pick + {cwd_last_focused_at: .cwd_last_focused_at})
      | sort_by(-.cwd_last_focused_at, .cwd)
      | .[]
      | [(.cwd|tostring), (.title|tostring), (.os_focused|tostring), (.tab_focused|tostring), (.win_id|tostring)]
      | @tsv
    '
  )"

  [[ -z "${sessions_tsv:-}" ]] && return 1

  printf "%s\n" "$sessions_tsv" | awk -F'\t' -v home="${HOME}" '{
    cwd=$1; title=$2; os_focused=$3; tab_focused=$4; win_id=$5
    display_path=cwd
    if (home != "" && index(cwd, home) == 1) {
      display_path = "~" substr(cwd, length(home) + 1)
    }
    idx=NR
    if (os_focused == "true" && tab_focused == "true") {
      marker="● "
    } else {
      marker="○ "
    }
    printf "%d\t%s\t%s%s  %s\n", idx, cwd, marker, title, display_path
  }'
}

default_mode="insert"
mode="$default_mode"
fzf_start_pos=""

while true; do
  menu_lines="$(build_menu_lines || true)"
  if [[ -z "${menu_lines:-}" ]]; then
    echo "No open windows found."; exit 1
  fi

  fzf_out=""; fzf_rc=0

  if [[ "$mode" == "normal" ]]; then
    set +e
    fzf_start_pos_opt=()
    if [[ -n "${fzf_start_pos:-}" && "$fzf_start_pos" -gt 1 ]]; then
      fzf_start_action="down"
      for ((i = 3; i <= fzf_start_pos; i++)); do fzf_start_action+="+down"; done
      fzf_start_pos_opt=(--bind "result:${fzf_start_action}")
    fi
    fzf_out="$(
      printf "%s\n" "$menu_lines" |
        fzf --ansi --height=100% --reverse \
          --header="Normal: j/k move, d close, enter open, i insert, esc quit" \
          --prompt="Open Windows > " \
          --no-multi --disabled \
          --with-nth=3.. \
          --expect=enter,d,i,esc \
          --bind 'j:down,k:up' \
          --bind 'enter:accept,d:accept,i:accept' \
          --bind 'esc:abort' \
          --no-clear
    )"
    fzf_rc=$?; fzf_start_pos=""; set -e
  else
    set +e
    fzf_out="$(
      printf "%s\n" "$menu_lines" |
        fzf --ansi --height=100% --reverse \
          --header="Insert: type to filter, enter open, esc normal" \
          --prompt="Open Windows > " \
          --no-multi \
          --with-nth=3.. \
          --expect=enter,esc \
          --bind 'enter:accept' \
          --bind 'esc:abort' \
          --no-clear
    )"
    fzf_rc=$?; set -e
  fi

  if [[ $fzf_rc -ne 0 && -z "${fzf_out:-}" ]]; then
    key="esc"; sel=""
  else
    key="$(printf "%s\n" "$fzf_out" | head -n1)"
    sel="$(printf "%s\n" "$fzf_out" | sed -n '2p' || true)"
  fi

  selected_cwd=""; selected_index=""
  if [[ -n "${sel:-}" ]]; then
    selected_index="$(printf "%s" "$sel" | awk -F'\t' '{print $1}')"
    selected_cwd="$(printf "%s" "$sel" | awk -F'\t' '{print $2}')"
  fi

  [[ "$mode" == "insert" && "$key" == "esc" ]] && { mode="normal"; continue; }
  [[ "$mode" == "normal" && "$key" == "esc" ]] && exit 0
  [[ "$mode" == "normal" && "$key" == "i" ]] && { mode="insert"; continue; }

  if [[ -z "${selected_cwd:-}" ]]; then
    [[ "$mode" == "normal" ]] && exit 0
    mode="normal"; continue
  fi

  if [[ "$mode" == "normal" && "$key" == "d" ]]; then
    if [[ "${selected_index:-}" =~ ^[0-9]+$ ]]; then
      total_lines="$(printf "%s\n" "$menu_lines" | awk 'END{print NR}')"
      if [[ -n "${total_lines:-}" && "$selected_index" -ge "$total_lines" ]]; then
        fzf_start_pos=$((selected_index - 1))
      else
        fzf_start_pos=$selected_index
      fi
      [[ "$fzf_start_pos" -lt 1 ]] && fzf_start_pos=1
    fi
    $KITTY close-tab --match "cwd:${selected_cwd}" --ignore-no-match 2>/dev/null || true
    continue
  fi

  if [[ "$key" == "enter" ]]; then
    win_id=$($KITTY ls 2>/dev/null | jq -r --arg cwd "$selected_cwd" '
      .[].tabs[].windows[] | select(.cwd == $cwd) | .id
    ' 2>/dev/null | head -1)
    [[ -n "$win_id" ]] && $KITTY focus-window --match "id:$win_id" 2>/dev/null
    exit 0
  fi

  [[ "$mode" == "insert" ]] && { mode="normal"; continue; }
  exit 0
done
