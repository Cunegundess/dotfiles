local ok, ghostty = pcall(require, 'ghostty-theme-sync')
if not ok then return end

ghostty.setup {
  theme_source = 'ghostty',
  colorscheme_fallback = 'catppuccin-mocha',
  disable_terminal_colors = false,
}
