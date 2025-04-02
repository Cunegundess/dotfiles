return {
  'projekt0n/github-nvim-theme',
  name = 'github-theme',
  config = function()
    require('github-theme').setup {
      styles = {
        comments = italic,
        functions = 'NONE',
        keywords = 'NONE',
        variables = 'NONE',
        conditionals = 'NONE',
        constants = 'NONE',
        numbers = 'NONE',
        operators = 'NONE',
        strings = italic,
        types = bold,
      },
    }
    -- vim.cmd.colorscheme 'github_dark_default'
  end,
}
