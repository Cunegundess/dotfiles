return {
  'datsfilipe/vesper.nvim',
  config = function()
    require('vesper').setup {
      transparent = false,
      italics = {
        comments = true,
        keywords = true,
        functions = true,
        strings = true,
        variables = true,
      },
      overrides = {}, -- A dictionary of group names, can be a function returning a dictionary or a table.
      palette_overrides = {},
    }
  end,
}
