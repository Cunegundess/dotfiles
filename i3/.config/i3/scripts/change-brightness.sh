#!/usr/bin/env bash

# Ajuste: up/down e valor de incremento
DIRECTION=$1
STEP=100 # valor absoluto (0-maximum), ex: 100 = ~10% se max=1000

BRIGHTNESS_PATH="/sys/class/backlight/intel_backlight"
MAX=$(cat "$BRIGHTNESS_PATH/max_brightness")
CUR=$(cat "$BRIGHTNESS_PATH/brightness")

if [[ $DIRECTION == "up" ]]; then
    NEW=$((CUR + STEP))
elif [[ $DIRECTION == "down" ]]; then
    NEW=$((CUR - STEP))
fi

# Limites
[[ $NEW -gt $MAX ]] && NEW=$MAX
[[ $NEW -lt 0 ]] && NEW=0

echo "$NEW" | sudo tee "$BRIGHTNESS_PATH/brightness" >/dev/null

# Percentual
PERCENT=$((NEW * 100 / MAX))
notify-send "Brightness: ${PERCENT}%"
