#!/usr/bin/env zsh

WALLPAPER_DIR="$HOME/wallpapers/"

while true; do
    ALL_WALLS=($(find "$WALLPAPER_DIR" -type f))

    CURRENT_WALL=$(swww query | grep 'Image path:' | cut -d':' -f2- | xargs)

    NEW_WALL=$(printf "%s\n" "${ALL_WALLS[@]}" | grep -vF "$CURRENT_WALL" | shuf -n 1)

    swww img "$NEW_WALL" --transition-type any

    sleep 60
done
