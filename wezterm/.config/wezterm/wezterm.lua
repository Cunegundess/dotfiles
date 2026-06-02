local wezterm = require("wezterm")
local config = wezterm.config_builder()
local act = wezterm.action
local mux = wezterm.mux

-- =============================================================================
-- Aparência
-- =============================================================================
config.color_scheme = "GitHub Dark"
config.font = wezterm.font("GeistMono NF", { weight = "Regular" })
config.font_size = 10
config.line_height = 1.5
config.default_cursor_style = "BlinkingBlock"

config.window_background_opacity = 0.9
config.window_padding = { left = 20, right = 20, top = 20, bottom = 20 }
config.window_frame = {
	font = wezterm.font("GeistMono NF", { weight = "Bold" }),
	font_size = 10,
}
config.initial_cols = 180
config.initial_rows = 37
config.enable_wayland = true

-- KDE Plasma: NONE deixa o KDE gerenciar as decorações (SSD)
config.window_decorations = "NONE"

config.enable_tab_bar = true
config.hide_tab_bar_if_only_one_tab = true
config.pane_focus_follows_mouse = true
config.detect_password_input = true

-- =============================================================================
-- Tab bar customizada (ícone + número + diretório)
-- =============================================================================
wezterm.on("format-tab-title", function(tab, _, _, _, _, _)
	local title = tab.tab_title
	if title == "" then
		title = tab.active_pane.title
	end

	local index = tab.tab_index + 1
	local is_active = tab.is_active

	local bg = is_active and "rgba(70,70,95,0.95)" or "rgba(35,35,45,0.85)"
	local fg = is_active and "#ffffff" or "#999999"
	local process = tab.active_pane.foreground_process_name or ""

	local icon = ""
	if process:find("nvim") then
		icon = ""
	elseif process:find("lazygit") then
		icon = " "
	elseif process:find("htop") then
		icon = "󰓅"
	elseif process:find("ssh") then
		icon = "󰣀"
	end

	local cwd = ""
	local cwd_uri = tab.active_pane.current_working_dir
	if cwd_uri then
		local path = tostring(cwd_uri)
		local name = path:match("/([^/]+)/?$")
		if name then
			cwd = " " .. name
		end
	end

	return {
		{ Background = { Color = bg } },
		{ Foreground = { Color = fg } },
		{ Text = "  " .. icon .. "  " .. index .. cwd .. "  " },
	}
end)

-- =============================================================================
-- Auto-título da aba baseado no diretório atual
-- =============================================================================
wezterm.on("pane-focus-changed", function(window, pane)
	local title = window:active_tab():get_title()
	if title == "" then
		local cwd = pane:get_current_working_dir()
		if cwd then
			local path = tostring(cwd)
			local name = path:match("/([^/]+)/?$")
			if name then
				window:active_tab():set_title(name)
			end
		end
	end
end)

-- =============================================================================
-- Keybindings: Workspaces (como sessões do tmux)
-- =============================================================================
local workspace_keys = {
	{ key = "LeftArrow", mods = "ALT", action = act.SwitchWorkspaceRelative(-1) },
	{ key = "RightArrow", mods = "ALT", action = act.SwitchWorkspaceRelative(1) },
	{ key = "w", mods = "ALT", action = act.ShowLauncherArgs({ flags = "WORKSPACES" }) },
	{
		key = "W",
		mods = "ALT|SHIFT",
		action = act.PromptInputLine({
			description = "New workspace name:",
			action = wezterm.action_callback(function(win, _, line)
				if line then
					win:switch_workspace(line)
				end
			end),
		}),
	},
}

-- =============================================================================
-- Keybindings: Panes (split, navegação, resize, zoom)
-- =============================================================================
local pane_keys = {
	{ key = "v", mods = "ALT", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
	{ key = "s", mods = "ALT", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
	{ key = "m", mods = "ALT", action = act.RotatePanes("Clockwise") },

	{ key = "h", mods = "ALT", action = act.ActivatePaneDirection("Left") },
	{ key = "l", mods = "ALT", action = act.ActivatePaneDirection("Right") },
	{ key = "k", mods = "ALT", action = act.ActivatePaneDirection("Up") },
	{ key = "j", mods = "ALT", action = act.ActivatePaneDirection("Down") },

	{ key = "H", mods = "ALT", action = act.AdjustPaneSize({ "Left", 5 }) },
	{ key = "J", mods = "ALT", action = act.AdjustPaneSize({ "Down", 5 }) },
	{ key = "K", mods = "ALT", action = act.AdjustPaneSize({ "Up", 5 }) },
	{ key = "L", mods = "ALT", action = act.AdjustPaneSize({ "Right", 5 }) },

	{ key = "z", mods = "ALT", action = act.TogglePaneZoomState },
}

-- =============================================================================
-- Keybindings: Abas
-- =============================================================================
local tab_keys = {
	{ key = "t", mods = "ALT", action = act.SpawnTab("DefaultDomain") },
	{ key = "n", mods = "ALT", action = act.ActivateTabRelative(1) },
	{ key = "p", mods = "ALT", action = act.ActivateTabRelative(-1) },
}

for i = 1, 8 do
	table.insert(tab_keys, { key = tostring(i), mods = "ALT", action = act.ActivateTab(i - 1) })
	table.insert(tab_keys, { key = tostring(i), mods = "ALT|SHIFT", action = act.MoveTab(i - 1) })
end

-- =============================================================================
-- Keybindings: Modos (Copy mode, Command palette, Quick select)
-- =============================================================================
local mode_keys = {
	{ key = "Y", mods = "ALT|SHIFT", action = act.ActivateCopyMode },
	{ key = "P", mods = "ALT|SHIFT", action = act.ActivateCommandPalette },
	{ key = "x", mods = "ALT", action = act.QuickSelect },
}

-- =============================================================================
-- Keybindings: Launcher fuzzy (navega entre abas, workspaces, domínios)
-- =============================================================================
local launcher_keys = {
	{ key = "Space", mods = "ALT", action = act.ShowLauncher },
}

-- =============================================================================
-- Keybindings: Panes flutuantes (display-popup style)
-- =============================================================================
local floating_keys = {
	{
		key = "S",
		mods = "ALT",
		action = act.SplitPane({
			direction = "Right",
			size = { Percent = 50 },
			top_level = true,
			command = { args = { "/home/lucas/.config/tmux/scripts/tmux-sessionizer" } },
		}),
	},
	{
		key = "g",
		mods = "ALT",
		action = act.SplitPane({
			direction = "Right",
			size = { Percent = 50 },
			top_level = true,
			command = { args = { "lazygit" } },
		}),
	},
	{
		key = "q",
		mods = "ALT",
		action = act.SplitPane({
			direction = "Right",
			size = { Percent = 50 },
			top_level = true,
			command = { args = { "htop" } },
		}),
	},
	{
		key = "N",
		mods = "ALT",
		action = act.SplitPane({
			direction = "Right",
			size = { Percent = 50 },
			top_level = true,
			command = {
				args = {
					"nvim",
					"-c",
					"cd ~/Documentos/notes",
					"/home/lucas/Documentos/notes/0-inbox/" .. os.date("%Y-%m-%d") .. ".md",
				},
			},
		}),
	},
}

-- =============================================================================
-- Session persistence (salva/restaura workspaces automaticamente)
-- =============================================================================
wezterm.on("gui-startup", function()
	local has_sessions, saved = pcall(wezterm.json_parse, wezterm.config_dir .. "/saved-sessions.json")
	if has_sessions and saved and saved.workspaces then
		for _, ws_name in ipairs(saved.workspaces) do
			mux.switch_workspace(ws_name)
		end
	end
end)

wezterm.on("window-closed", function(window, _)
	local tabs = mux.all_tabs()
	local ws_names = {}
	for _, tab in ipairs(tabs) do
		local ws = tab:workspace()
		if ws and ws ~= "default" then
			ws_names[ws] = true
		end
	end
	local result = {}
	for name, _ in pairs(ws_names) do
		table.insert(result, name)
	end
	local data = wezterm.json_encode({ workspaces = result })
	local f = io.open(wezterm.config_dir .. "/saved-sessions.json", "w")
	if f then
		f:write(data)
		f:close()
	end
end)

-- =============================================================================
-- Montagem de todas as keys
-- =============================================================================
config.keys = {}
local all_keys = { pane_keys, tab_keys, mode_keys, launcher_keys, workspace_keys, floating_keys }
for _, tbl in ipairs(all_keys) do
	for _, key in ipairs(tbl) do
		table.insert(config.keys, key)
	end
end

return config
