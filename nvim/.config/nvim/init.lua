vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

vim.pack.add({
  -- Theme
  'https://github.com/datsfilipe/vesper.nvim',

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
  'https://github.com/lewis6991/gitsigns.nvim',

  -- Navigation
  'https://github.com/stevearc/oil.nvim',
  'https://github.com/stevearc/quicker.nvim',

  -- Diff
  'https://github.com/barrettruth/diffs.nvim',
})

require('theme')
