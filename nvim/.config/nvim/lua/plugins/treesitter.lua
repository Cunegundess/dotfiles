require('nvim-treesitter.configs').setup {
  ensure_installed = {
    'c', 'cpp', 'go', 'rust', 'cmake', 'bash',
    'lua', 'vim', 'vimdoc',
    'html', 'css', 'javascript', 'typescript', 'tsx',
    'json', 'yaml', 'toml', 'markdown', 'markdown_inline',
    'python', 'java', 'kotlin',
  },
  sync_install = false,
  auto_install = true,
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = true,
  },
  indent = { enable = true },
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
