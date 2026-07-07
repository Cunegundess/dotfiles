vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

vim.pack.add({
  -- Core
  'https://github.com/echasnovski/mini.nvim',

  -- Theme
  'https://github.com/datsfilipe/vesper.nvim',

  -- LSP & Completion
  'https://github.com/saghen/blink.cmp',
  'https://github.com/neovim/nvim-lspconfig',

  -- DAP
  'https://github.com/mfussenegger/nvim-dap',
  'https://github.com/rcarriga/nvim-dap-ui',
  'https://github.com/nvim-neotest/nvim-nio',
  'https://github.com/mfussenegger/nvim-dap-python',

  -- Dadbod
  'https://github.com/tpope/vim-dadbod',
  'https://github.com/kristijanhusak/vim-dadbod-ui',
  'https://github.com/kristijanhusak/vim-dadbod-completion',

  -- Picker
  'https://github.com/ibhagwan/fzf-lua',
  'https://github.com/nvim-lua/plenary.nvim',

  -- Git
  'https://github.com/tpope/vim-fugitive',
  'https://github.com/tpope/vim-rhubarb',
  'https://github.com/lewis6991/gitsigns.nvim',
  'https://github.com/justinmk/guh.nvim',

  -- tpope essentials
  'https://github.com/tpope/vim-repeat',
  'https://github.com/tpope/vim-eunuch',
  'https://github.com/tpope/vim-rsi',
  'https://github.com/tpope/vim-unimpaired',
  'https://github.com/tpope/vim-dispatch',

  -- Justin's utils
  'https://github.com/justinmk/vim-sneak.git',
  'https://github.com/justinmk/vim-ipmotion.git',
  'https://github.com/justinmk/vim-gtfo.git',
  'https://github.com/justinmk/nvim-repl',
  'https://github.com/justinmk/vim-ug',

  -- Text objects & editing
  'https://github.com/tommcdo/vim-lion',
  'https://github.com/tommcdo/vim-exchange',
  'https://github.com/haya14busa/vim-edgemotion',
  'https://github.com/mfussenegger/nvim-treehopper',
  'https://github.com/AndrewRadev/linediff.vim',

  -- Navigation
  'https://github.com/stevearc/oil.nvim',
  'https://github.com/stevearc/quicker.nvim',
  'https://github.com/mfussenegger/nvim-qwahl',

  -- Diff
  'https://github.com/barrettruth/diffs.nvim',

  -- Misc
  'https://github.com/folke/zen-mode.nvim',
  'https://github.com/tpope/vim-obsession',
  'https://github.com/tpope/vim-projectionist',
  'https://github.com/tpope/vim-characterize',
  'https://github.com/tpope/vim-apathy',
})

vim.pack.add({
  'https://github.com/chrisbra/Colorizer',
  'https://github.com/mbbill/undotree',
  'https://github.com/MeanderingProgrammer/render-markdown.nvim',
}, {
  load = function() end,
})

require('theme')
