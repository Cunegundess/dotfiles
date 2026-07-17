#!/usr/bin/env bash
# Segmento de git para a tmux status bar.
# Mostra: branch atual, e um indicador quando há mudanças não commitadas.
# Fica em silêncio (sem output) quando o diretório não é um repo git.

dir="$1"
cd "$dir" 2>/dev/null || exit 0

git rev-parse --is-inside-work-tree >/dev/null 2>&1 || exit 0

branch=$(git symbolic-ref --short HEAD 2>/dev/null || git rev-parse --short HEAD 2>/dev/null)
[ -z "$branch" ] && exit 0

if [ -n "$(git status --porcelain 2>/dev/null)" ]; then
  echo " ${branch} ●"
else
  echo " ${branch}"
fi
