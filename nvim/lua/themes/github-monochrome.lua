return {
  'idr4n/github-monochrome.nvim',
  lazy = false,
  priority = 1000,
  opts = {},
  config = function()
    require('github-monochrome').setup {
      styles = {
        comments = { italic = true },
        conditionals = { bold = true },
        loops = { bold = true },
        variables = {},
        floats = 'dark',
        sidebars = 'dark',
      },
    }
  end,
}
