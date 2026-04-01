require('nvim-treesitter.configs').setup {
  ensure_installed = {
    'lua', 'vim', 'vimdoc',
    'python', 'javascript', 'typescript', 'tsx',
    'html', 'css', 'json', 'yaml', 'toml',
    'markdown', 'markdown_inline',
    'bash', 'c', 'cpp', 'go', 'rust', 'cmake',
  },
  sync_install = false,
  auto_install = true,
  highlight = {
    enable = true,
  },
  indent = {
    enable = true,
  },
  textobjects = {
    select = {
      enable = true,
      lookahead = true,
      keymaps = {
        ['af'] = '@function.outer',
        ['if'] = '@function.inner',
        ['ac'] = '@class.outer',
        ['ic'] = '@class.inner',
      },
    },
  },
}

require('treesitter-context').setup {
  enable = true,
  max_lines = 3,
}
