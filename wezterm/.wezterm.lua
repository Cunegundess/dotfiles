local wezterm = require("wezterm")
local config = wezterm.config_builder()

-- config.color_scheme = "AdventureTime"
config.font = wezterm.font("JetBrains Mono Nerd Font", { weight = "Bold" })
config.font_size = 16
config.enable_tab_bar = false

return config
