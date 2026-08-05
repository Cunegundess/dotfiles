vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

vim.pack.add({
  -- Theme
  'https://github.com/datsfilipe/vesper.nvim',
  'https://github.com/folke/tokyonight.nvim',

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

  -- Mason (LSP/DAP installer)
  'https://github.com/williamboman/mason.nvim',

  -- Treesitter
  'https://github.com/nvim-treesitter/nvim-treesitter',

  -- Render Markdown
  'https://github.com/MeanderingProgrammer/render-markdown.nvim.git',
})

require('mason').setup()

vim.schedule(function()
  local ok, registry = pcall(require, 'mason-registry')
  if not ok then return end
  local packages = {
    'jdtls', 'java-debug-adapter', 'java-test',
    'kotlin-language-server', 'kotlin-debug-adapter',
    'basedpyright', 'ruff', 'lua-language-server',
    'bash-language-server',
  }
  for _, name in ipairs(packages) do
    local ok_pkg, pkg = pcall(registry.get_package, registry, name)
    if ok_pkg and not pkg:is_installed() then
      pkg:install()
    end
  end
end)

require('nvim-treesitter.config').setup({
  ensure_installed = { 'java', 'kotlin' },
  auto_install = true,
  highlight = { enable = true },
})
require('theme')
