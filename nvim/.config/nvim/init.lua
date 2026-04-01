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
  -- Themes
  'catppuccin/nvim',
  'folke/tokyonight.nvim',
  'rose-pine/neovim',
  'rebelot/kanagawa.nvim',
  'shaunsingh/nord.nvim',
  'folke/twilight.nvim',
  'folke/zen-mode.nvim',

  -- LSP & Completion
  'hrsh7th/nvim-cmp',
  'L3MON4D3/LuaSnip',
  'saadparwaiz1/cmp_luasnip',
  'hrsh7th/cmp-nvim-lsp',
  'hrsh7th/cmp-path',

  -- UI (mini.nvim substitui: surround, pairs, comment, indentscope, move, etc)
  'echasnovski/mini.nvim',
  'echasnovski/mini.icons',
  'folke/which-key.nvim',

  -- File navigation
  'stevearc/oil.nvim',
  'ThePrimeagen/harpoon',
  'nvim-lua/plenary.nvim',
  'ibhagwan/fzf-lua',

  -- Git
  'lewis6991/gitsigns.nvim',

  -- Formatting
  'stevearc/conform.nvim',
  'williamboman/mason.nvim',
  'WhoIsSethDaniel/mason-tool-installer.nvim',

  -- Syntax
  'nvim-treesitter/nvim-treesitter',
  'nvim-treesitter/nvim-treesitter-textobjects',
  'nvim-treesitter/nvim-treesitter-context',

  -- Debug
  'mfussenegger/nvim-dap',
  'rcarriga/nvim-dap-ui',
  'nvim-neotest/nvim-nio',
  'mfussenegger/nvim-dap-python',
}

for _, plugin in ipairs(plugins) do
  clone(plugin)
end

require 'plugins'
