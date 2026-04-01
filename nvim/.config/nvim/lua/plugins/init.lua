vim.defer_fn(function()
  pcall(require, 'plugins.catppuccin')
  pcall(require, 'plugins.cmp')
  pcall(require, 'plugins.treesitter')
  pcall(require, 'plugins.mini')
  pcall(require, 'plugins.lualine')
  pcall(require, 'plugins.whichkey')
  pcall(require, 'plugins.conform')
  pcall(require, 'plugins.gitsigns')
  pcall(require, 'plugins.fzf')
  pcall(require, 'plugins.oil')
  pcall(require, 'plugins.ghostty_theme_sync')

  local ok_dap, dap_config = pcall(require, 'plugins.dap')
  if ok_dap and type(dap_config) == 'function' then
    dap_config()
  end

  require('config.colorscheme').apply()
end, 0)
