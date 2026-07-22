#!/usr/bin/env bash
# Git status for kitty tab bar
# Shows: branch name + ● if dirty

dir="${1:-$(pwd)}"
cd "$dir" 2>/dev/null || exit 0

git rev-parse --is-inside-work-tree >/dev/null 2>&1 || exit 0

branch=$(git symbolic-ref --short HEAD 2>/dev/null || git rev-parse --short HEAD 2>/dev/null)
[ -z "$branch" ] && exit 0

if [ -n "$(git status --porcelain 2>/dev/null)" ]; then
  printf '%s●' "$branch"
else
  printf '%s' "$branch"
fi
