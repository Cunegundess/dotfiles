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

  {
    src = 'https://github.com/JavaHello/spring-boot.nvim',
    version = '218c0c26c14d99feca778e4d13f5ec3e8b1b60f0',
  },

  'https://github.com/MunifTanjim/nui.nvim',
  'https://github.com/nvim-java/nvim-java',
})

require('java').setup()
vim.lsp.enable('jdtls')

require('theme')
