vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

require 'config'
require 'keymaps'
require 'autocmds'
require 'lsp'

local function clone(repo)
  local name = repo:match '[^/]+$'
  local path = vim.fn.stdpath 'data' .. '/site/pack/vendor/start/' .. name
  if vim.fn.isdirectory(path) == 0 then
    vim.fn.system { 'git', 'clone', '--depth', '1', 'https://github.com/' .. repo, path }
  end
end

local plugins = {
  'catppuccin/nvim',
  'hrsh7th/nvim-cmp',
  'L3MON4D3/LuaSnip',
  'saadparwaiz1/cmp_luasnip',
  'hrsh7th/cmp-nvim-lsp',
  'hrsh7th/cmp-path',
  'nvim-lualine/lualine.nvim',
  'nvim-tree/nvim-web-devicons',
  'ThePrimeagen/harpoon',
  'nvim-lua/plenary.nvim',
  'echasnovski/mini.nvim',
  'folke/which-key.nvim',
  'stevearc/conform.nvim',
  'ibhagwan/fzf-lua',
  'echasnovski/mini.icons',
  'lewis6991/gitsigns.nvim',
  'williamboman/mason.nvim',
  'WhoIsSethDaniel/mason-tool-installer.nvim',
  'nvim-treesitter/nvim-treesitter',
  'nvim-treesitter/nvim-treesitter-textobjects',
  'nvim-treesitter/nvim-treesitter-context',
  'mfussenegger/nvim-dap',
  'rcarriga/nvim-dap-ui',
  'nvim-neotest/nvim-nio',
  'mfussenegger/nvim-dap-python',
  'stevearc/oil.nvim'
}

for _, plugin in ipairs(plugins) do
  clone(plugin)
end

vim.defer_fn(function()
  pcall(require, 'plugins.cmp')
  pcall(require, 'plugins.treesitter')
  pcall(require, 'plugins.lualine')
  pcall(require, 'plugins.mini')
  pcall(require, 'plugins.whichkey')
  pcall(require, 'plugins.conform')
  pcall(require, 'plugins.dap')
  pcall(require, 'plugins.fzf')
  pcall(require, 'plugins.gitsigns')
  pcall(require, 'plugins.catppuccin')
  pcall(require, 'plugins.oil')
end, 0)
