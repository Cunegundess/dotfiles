return {
  'catppuccin/nvim',
  name = 'catppuccin',
  lazy = false,
  priority = 1000,
  opts = {
    transparent_background = true,
    flavour = 'mocha',
    term_colors = true,
    no_italics = true,
    styles = {
      comments = {},
      conditionals = {},
    },
    custom_highlights = function(colors)
      return {
        WinSeparator = { fg = colors.surface0 },
      }
    end,
    color_overrides = {
      mocha = {
        base = '#11111b',
        mantle = '#11111b',
      },
    },
    integrations = {
      notify = true,
    },
  },
  init = function() end,
}
