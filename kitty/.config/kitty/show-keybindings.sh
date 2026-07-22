#!/usr/bin/env bash
# Kitty keybindings cheatsheet — shown as an overlay
# Trigger: kitty_mod+/

parse_kitty_conf() {
    local conf="$HOME/.config/kitty/kitty.conf"
    if [[ -f "$conf" ]]; then
        awk '
        /^map / && !/^#/ {
            # Remove leading "map "
            sub(/^map /, "")
            # Split into key and action
            key = $1
            action = ""
            for (i=2; i<=NF; i++) action = action (i>2 ? " " : "") $i
            # Shorten common modifiers
            gsub(/kitty_mod/, "Mod", key)
            gsub(/ctrl\+shift\+/, "CS+", key)
            gsub(/ctrl\+/, "C+", key)
            gsub(/alt\+/, "A+", key)
            gsub(/shift\+/, "S+", key)
            # Skip navigation-only or internal keys
            if (action ~ /^(discard_event|pass_selection_to_program)$/) next
            printf "  %-28s %s\n", key, action
        }
        ' "$conf"
    fi
}

print_cheatsheet() {
    printf "\n"
    printf "  \033[1;36m═══════════════════════════════════════════════\033[0m\n"
    printf "  \033[1;37m         Kitty Keybindings Cheatsheet\033[0m\n"
    printf "  \033[1;36m═══════════════════════════════════════════════\033[0m\n"
    printf "\n"

    printf "  \033[1;33m── Custom (kitty.conf) ──────────────────────\033[0m\n"
    local custom
    custom=$(parse_kitty_conf)
    if [[ -n "$custom" ]]; then
        printf "%s\n" "$custom"
    else
        printf "  (no custom keybindings)\n"
    fi
    printf "\n"

    printf "  \033[1;33m── Splits ───────────────────────────────────\033[0m\n"
    printf "  %-28s %s\n" "Mod+\\" "Split horizontal"
    printf "  %-28s %s\n" "Mod+-" "Split vertical"
    printf "\n"

    printf "  \033[1;33m── Navigation ───────────────────────────────\033[0m\n"
    printf "  %-28s %s\n" "Mod+arrows" "Navigate panes"
    printf "  %-28s %s\n" "Ctrl+arrows" "Resize panes"
    printf "  %-28s %s\n" "Mod+p/n" "Prev/next window"
    printf "  %-28s %s\n" "Mod+[/" "Prev/next window"
    printf "  %-28s %s\n" "Mod+1-9" "Go to window N"
    printf "\n"

    printf "  \033[1;33m── Tabs ─────────────────────────────────────\033[0m\n"
    printf "  %-28s %s\n" "Mod+t" "New tab"
    printf "  %-28s %s\n" "Mod+shift+q" "Close tab"
    printf "  %-28s %s\n" "Mod+right/left" "Next/prev tab"
    printf "  %-28s %s\n" "Mod+shift+." "Move tab forward"
    printf "  %-28s %s\n" "Mod+shift+," "Move tab backward"
    printf "\n"

    printf "  \033[1;33m── Sessions ─────────────────────────────────\033[0m\n"
    printf "  %-28s %s\n" "Mod+s" "Session switcher (kitten sessions)"
    printf "\n"

    printf "  \033[1;33m── Windows ──────────────────────────────────\033[0m\n"
    printf "  %-28s %s\n" "Mod+enter" "New window"
    printf "  %-28s %s\n" "Mod+shift+w" "Close window"
    printf "  %-28s %s\n" "Mod+f" "Move window forward"
    printf "  %-28s %s\n" "Mod+b" "Move window backward"
    printf "  %-28s %s\n" "Mod+\`" "Move window to top"
    printf "\n"

    printf "  \033[1;33m── Scrolling ────────────────────────────────\033[0m\n"
    printf "  %-28s %s\n" "Mod+up/down" "Scroll line"
    printf "  %-28s %s\n" "Mod+page_up/down" "Scroll page"
    printf "  %-28s %s\n" "Mod+home/end" "Scroll to top/bottom"
    printf "  %-28s %s\n" "Mod+z/x" "Prev/next prompt"
    printf "  %-28s %s\n" "Mod+g" "Last command output"
    printf "\n"

    printf "  \033[1;33m── Overlay ──────────────────────────────────\033[0m\n"
    printf "  %-28s %s\n" "Mod+/" "This cheatsheet"
    printf "  %-28s %s\n" "Mod+e (overlay)" "Open URL with hints"
    printf "  %-28s %s\n" "Mod+p>f" "Insert path hint"
    printf "  %-28s %s\n" "Mod+p>l" "Insert line hint"
    printf "  %-28s %s\n" "Mod+p>w" "Insert word hint"
    printf "\n"

    printf "  \033[1;33m── Clipboard ────────────────────────────────\033[0m\n"
    printf "  %-28s %s\n" "Mod+c" "Copy to clipboard"
    printf "  %-28s %s\n" "Mod+v" "Paste from clipboard"
    printf "  %-28s %s\n" "Mod+s" "Paste from selection"
    printf "\n"

    printf "  \033[1;33m── Misc ────────────────────────────────────\033[0m\n"
    printf "  %-28s %s\n" "Mod+escape" "Kitty shell"
    printf "  %-28s %s\n" "Mod+shift+," "Reload config"
    printf "  %-28s %s\n" "Mod+f11" "Fullscreen"
    printf "  %-28s %s\n" "Mod+f10" "Maximized"
    printf "  %-28s %s\n" "Mod+f6" "Debug config"
    printf "  %-28s %s\n" "Mod+equal/minus" "Font size +/-"
    printf "  %-28s %s\n" "Mod+backspace" "Reset font size"
    printf "  %-28s %s\n" "Mod+f2" "Edit config file"
    printf "\n"

    printf "  \033[1;33m── Layouts ──────────────────────────────────\033[0m\n"
    printf "  %-28s %s\n" "Mod+l" "Next layout"
    printf "  %-28s %s\n" "Mod+shift+l" "Cycle layouts"
    printf "  %-28s %s\n" "Mod+alt+t" "Go to layout tall"
    printf "  %-28s %s\n" "Mod+alt+g" "Go to layout grid"
    printf "  %-28s %s\n" "Mod+alt+h" "Go to layout horizontal"
    printf "  %-28s %s\n" "Mod+alt+v" "Go to layout vertical"
    printf "  %-28s %s\n" "Mod+alt+s" "Go to layout stack"
    printf "  %-28s %s\n" "Mod+alt+p" "Last used layout"
    printf "  %-28s %s\n" "Mod+alt+z" "Toggle layout stack (zoom)"
    printf "\n"

    printf "  \033[1;36m═══════════════════════════════════════════════\033[0m\n"
    printf "  \033[2mPress any key or move mouse to dismiss\033[0m\n"
    printf "\n"
}

print_cheatsheet
