require 'lsp'
require 'core.config'
require 'core.keymaps'
require 'core.autocmds'
require 'core.commands'

-- [[ Configure lazy.nvim ]]
local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = 'https://github.com/folke/lazy.nvim.git'
  local out = vim.fn.system { 'git', 'clone', '--filter=blob:none', '--branch=stable', lazyrepo, lazypath }
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { 'Failed to clone lazy.nvim:\n', 'ErrorMsg' },
      { out, 'WarningMsg' },
      { '\nPress any key to exit...' },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end ---@diagnostic disable-next-line: undefined-field
vim.opt.rtp:prepend(lazypath)

-- [[ Configure and install plugins ]]
require('lazy').setup({
  { import = 'plugins' },
  { import = 'themes' },
}, {
  ui = {
    icons = vim.g.have_nerd_font and {} or {
      cmd = '⌘',
      config = '🛠',
      event = '📅',
      ft = '📂',
      init = '⚙',
      keys = '🗝',
      plugin = '🔌',
      runtime = '💻',
      require = '🌙',
      source = '📄',
      start = '🚀',
      task = '📌',
      lazy = '💤 ',
    },
  },
})

-- [[ Set selected colorscheme ]]
local ok, theme = pcall(require, 'colorscheme')
if ok and theme then
  vim.cmd.colorscheme(theme)
else
  vim.cmd.colorscheme 'default'
end

function _G.save_colorscheme(name)
  local file = vim.fn.stdpath 'config' .. '/lua/colorscheme.lua'
  local f = io.open(file, 'w')
  if f then
    f:write(string.format('return "%s"\n', name))
    f:close()
    vim.notify("Colorscheme set to '" .. name .. "'", vim.log.levels.INFO)
  else
    vim.notify('Failed to save colorscheme', vim.log.levels.ERROR)
  end
end

-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 et
