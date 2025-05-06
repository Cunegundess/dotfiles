local wezterm = require("wezterm")
local config = wezterm.config_builder()

-- config.color_scheme = "AdventureTime"
config.font = wezterm.font("JetBrains Mono Nerd Font", { weight = "Bold" })
config.font_size = 12
config.line_height = 1.2
config.default_cursor_style = "BlinkingBlock"
config.enable_tab_bar = true
config.hide_tab_bar_if_only_one_tab = true
config.pane_focus_follows_mouse = true
config.detect_password_input = true
config.window_background_opacity = 0.9
config.text_background_opacity = 1.0
config.window_decorations = "RESIZE"
config.window_padding = {
	left = 10,
	right = 10,
	top = 10,
	bottom = 10,
}

config.initial_cols = 180
config.initial_rows = 37

config.max_fps = 120

config.colors = {
	foreground = "white",
}

config.window_frame = {
	font = wezterm.font("JetBrains Mono Nerd Font", { weight = "Bold" }),
	font_size = 11,
	inactive_titlebar_bg = "#000000",
	active_titlebar_bg = "#000000",
	inactive_titlebar_fg = "#cccccc",
	active_titlebar_fg = "#ffffff",
	inactive_titlebar_border_bottom = "#000000",
	active_titlebar_border_bottom = "#000000",
	button_fg = "#cccccc",
	button_bg = "#000000",
	button_hover_fg = "#ffffff",
	button_hover_bg = "#000000",
}

config.keys = {
	{
		key = "v",
		mods = "ALT",
		action = wezterm.action.SplitVertical({ domain = "CurrentPaneDomain" }),
	},
	{
		key = "h",
		mods = "ALT",
		action = wezterm.action.SplitHorizontal({ domain = "CurrentPaneDomain" }),
	},
	{
		key = "t",
		mods = "ALT",
		action = wezterm.action.SpawnTab("DefaultDomain"),
	},
	{
		key = "Y",
		mods = "ALT|SHIFT",
		action = wezterm.action.ActivateCopyMode,
	},
	{
		key = "P",
		mods = "ALT|SHIFT",
		action = wezterm.action.ActivateCommandPalette,
	},
	{
		key = "m",
		mods = "ALT",
		action = wezterm.action.RotatePanes("Clockwise"),
	},
	{
		key = "h",
		mods = "CTRL",
		action = wezterm.action.ActivatePaneDirection("Left"),
	},
	{
		key = "l",
		mods = "CTRL",
		action = wezterm.action.ActivatePaneDirection("Right"),
	},
	{
		key = "k",
		mods = "CTRL",
		action = wezterm.action.ActivatePaneDirection("Up"),
	},
	{
		key = "j",
		mods = "CTRL",
		action = wezterm.action.ActivatePaneDirection("Down"),
	},
}

-- Switch Tabs
for i = 1, 8 do
	table.insert(config.keys, {
		key = tostring(i),
		mods = "ALT",
		action = wezterm.action.ActivateTab(i - 1),
	})
end

-- Move Tabs
for i = 1, 8 do
	table.insert(config.keys, {
		key = tostring(i),
		mods = "ALT|SHIFT",
		action = wezterm.action.MoveTab(i - 1),
	})
end

return config
