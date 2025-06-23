return {
  'catppuccin/nvim',
  name = 'catppuccin',
  priority = 1000,
  opts = {
    flavour = 'mocha',
    term_colors = true,
    styles = {
      comments = { 'italic' },
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
  init = function()
    vim.cmd.colorscheme 'catppuccin'
  end,
}
