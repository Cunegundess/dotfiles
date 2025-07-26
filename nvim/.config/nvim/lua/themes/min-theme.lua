return {
  'datsfilipe/min-theme.nvim',
  config = function()
    require('min-theme').setup {
      theme = 'dark', -- String: 'dark' or 'light', determines the colorscheme used
      transparent = true,
      italics = {
        comments = true,
        keywords = true,
        functions = true,
        strings = true,
        variables = true,
      },
      overrides = {}, -- A dictionary of group names, can be a function returning a dictionary or a table.
    }
  end,
}
