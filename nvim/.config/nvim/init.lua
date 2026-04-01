vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

require 'config'
require 'keymaps'
require 'autocmds'
require 'lsp'

local function plugins()
  local pack = vim.pack

  pack.add {
    opt = false,
    { 'catppuccin/nvim', name = 'catppuccin' },
  }

  pack.add {
    opt = false,
    {
      'hrsh7th/nvim-cmp',
      dependencies = {
        { 'L3MON4D3/LuaSnip' },
        { 'saadparwaiz1/cmp_luasnip' },
        { 'hrsh7th/cmp-nvim-lsp' },
        { 'hrsh7th/cmp-path' },
      },
      event = 'InsertEnter',
    },
  }

  pack.add {
    opt = false,
    {
      'nvim-treesitter/nvim-treesitter',
      build = ':TSUpdate',
      dependencies = {
        'nvim-treesitter/nvim-treesitter-textobjects',
        'nvim-treesitter/nvim-treesitter-context',
      },
    },
  }

  pack.add {
    opt = false,
    { 'nvim-lualine/lualine.nvim', dependencies = { 'nvim-tree/nvim-web-devicons' } },
  }

  pack.add {
    opt = false,
    { 'ThePrimeagen/harpoon', dependencies = { 'nvim-lua/plenary.nvim' } },
  }

  pack.add {
    opt = false,
    {
      'echasnovski/mini.nvim',
      dependencies = {},
    },
  }

  pack.add {
    opt = false,
    {
      'folke/which-key.nvim',
      event = 'VimEnter',
      lazy = false,
      priority = 1000,
    },
  }

  pack.add {
    opt = false,
    {
      'stevearc/conform.nvim',
      event = { 'BufWritePre' },
      cmd = { 'ConformInfo' },
    },
  }

  pack.add {
    opt = false,
    {
      'mfussenegger/nvim-dap',
      dependencies = {
        'rcarriga/nvim-dap-ui',
        'nvim-neotest/nvim-nio',
        'mfussenegger/nvim-dap-python',
      },
    },
  }

  pack.add {
    opt = false,
    { 'ibhagwan/fzf-lua', dependencies = { 'echasnovski/mini.icons' } },
  }

  pack.add {
    opt = false,
    { 'lewis6991/gitsigns.nvim' },
  }

  pack.add {
    opt = false,
    { 'williamboman/mason.nvim' },
    { 'WhoIsSethDaniel/mason-tool-installer.nvim' },
  }
end

plugins()

vim.defer_fn(function()
  pcall(function()
    require('config.colorscheme').apply()
  end)
end, 0)
