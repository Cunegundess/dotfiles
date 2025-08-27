local wezterm = require("wezterm")
local config = wezterm.config_builder()

-- Tema e Fontes
-- config.color_scheme = "Vesper"
config.color_scheme = "Vacuous 2 (terminal.sexy)"
configsfont = wezterm.font("JetBrains Mono Nerd Font", { weight = "Bold" })
-- config.font = wezterm.font("Cascadia Code NF", { weight = "Regular" })
-- config.font = wezterm.font("GeistMono NF", { weight = "Regular" })
config.font_size = 10
config.line_height = 1.5
config.default_cursor_style = "BlinkingBlock"
-- config.text_background_opacity = 0.0

-- Janela e Layout
config.window_decorations = "RESIZE"
-- config.window_background_opacity = 0.9
config.window_padding = { left = 20, right = 20, top = 20, bottom = 20 }
config.window_frame = { font = wezterm.font("JetBrains Mono Nerd Font", { weight = "Bold" }), font_size = 11 }
config.initial_cols = 180
config.initial_rows = 37

-- Barra de abas e comportamento
config.enable_tab_bar = true
config.hide_tab_bar_if_only_one_tab = true
config.pane_focus_follows_mouse = true
config.detect_password_input = true

-- Keybindings: Panes
local pane_keys = {
	{ key = "v", mods = "ALT", action = wezterm.action.SplitVertical({ domain = "CurrentPaneDomain" }) },
	{ key = "/", mods = "ALT", action = wezterm.action.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
	{ key = "m", mods = "ALT", action = wezterm.action.RotatePanes("Clockwise") },
	{ key = "h", mods = "ALT", action = wezterm.action.ActivatePaneDirection("Left") },
	{ key = "l", mods = "ALT", action = wezterm.action.ActivatePaneDirection("Right") },
	{ key = "k", mods = "ALT", action = wezterm.action.ActivatePaneDirection("Up") },
	{ key = "j", mods = "ALT", action = wezterm.action.ActivatePaneDirection("Down") },
}

-- Keybindings: Tabs e modos
local tab_keys = {
	{ key = "t", mods = "ALT", action = wezterm.action.SpawnTab("DefaultDomain") },
	{ key = "Y", mods = "ALT|SHIFT", action = wezterm.action.ActivateCopyMode },
	{ key = "P", mods = "ALT|SHIFT", action = wezterm.action.ActivateCommandPalette },
}

for i = 1, 8 do
	table.insert(tab_keys, { key = tostring(i), mods = "ALT", action = wezterm.action.ActivateTab(i - 1) })
	table.insert(tab_keys, { key = tostring(i), mods = "ALT|SHIFT", action = wezterm.action.MoveTab(i - 1) })
end

config.keys = {}
for _, key in ipairs(pane_keys) do
	table.insert(config.keys, key)
end
for _, key in ipairs(tab_keys) do
	table.insert(config.keys, key)
end

return config
