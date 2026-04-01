vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

require 'config'
require 'keymaps'
require 'autocmds'
require 'lsp'

local function add_plugins()
  local function p(repo)
    return { src = 'https://github.com/' .. repo, stage = 'start' }
  end

  vim.pack.add({
    p 'catppuccin/nvim',
    p 'hrsh7th/nvim-cmp',
    p 'L3MON4D3/LuaSnip',
    p 'saadparwaiz1/cmp_luasnip',
    p 'hrsh7th/cmp-nvim-lsp',
    p 'hrsh7th/cmp-path',
    p 'nvim-lualine/lualine.nvim',
    p 'nvim-tree/nvim-web-devicons',
    p 'ThePrimeagen/harpoon',
    p 'nvim-lua/plenary.nvim',
    p 'echasnovski/mini.nvim',
    p 'folke/which-key.nvim',
    p 'stevearc/conform.nvim',
    p 'ibhagwan/fzf-lua',
    p 'echasnovski/mini.icons',
    p 'lewis6991/gitsigns.nvim',
    p 'williamboman/mason.nvim',
    p 'WhoIsSethDaniel/mason-tool-installer.nvim',
    p 'nvim-treesitter/nvim-treesitter',
    p 'nvim-treesitter/nvim-treesitter-textobjects',
    p 'nvim-treesitter/nvim-treesitter-context',
    p 'mfussenegger/nvim-dap',
    p 'rcarriga/nvim-dap-ui',
    p 'nvim-neotest/nvim-nio',
    p 'mfussenegger/nvim-dap-python',
  })
end

add_plugins()

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
end, 0)
