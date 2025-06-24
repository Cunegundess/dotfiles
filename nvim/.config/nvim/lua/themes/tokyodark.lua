return {
  'tiagovla/tokyodark.nvim',
  lazy = false,
  priority = 1000,
  opts = {
    transparent_background = false,
    gamma = 1.00, -- adjust the brightness of the theme
    styles = {
      comments = { italic = false },
      keywords = { italic = false },
      identifiers = { italic = false },
      functions = {},
      variables = {},
    },
  },
}
