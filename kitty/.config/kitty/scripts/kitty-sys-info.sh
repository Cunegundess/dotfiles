#!/usr/bin/env bash
# CPU + RAM for kitty tab bar (Linux)
# Uses cache to avoid blocking the tab bar refresh

cache="/tmp/.kitty_sysinfo_cache"

read -r _ u1 n1 s1 i1 iw1 irq1 sirq1 _ < /proc/stat
total1=$((u1 + n1 + s1 + i1 + iw1 + irq1 + sirq1))
idle1=$i1

if [ -f "$cache" ]; then
  read -r total0 idle0 < "$cache" 2>/dev/null || { total0=$total1; idle0=$idle1; }
else
  total0=$total1
  idle0=$idle1
fi
echo "$total1 $idle1" > "$cache"

totald=$((total1 - total0))
idled=$((idle1 - idle0))

if [ "$totald" -le 0 ]; then
  cpu=0
else
  cpu=$(( (100 * (totald - idled)) / totald ))
fi

mem=$(awk '/MemTotal/{t=$2} /MemAvailable/{a=$2} END{printf "%.1f/%.1fG", (t-a)/1024/1024, t/1024/1024}' /proc/meminfo)

printf '%s %s' "$cpu%" "$mem"
