return {
  'dgox16/oldworld.nvim',
  lazy = false,
  priority = 1000,
  opts = {
    variant = 'default', -- default, oled, cooler
    integrations = { -- You can disable/enable integrations
      neo_tree = true,
      neogit = true,
    },
    styles = { -- You can pass the style using the format: style = true
      comments = { italic = true }, -- style for comments
      keywords = { italic = true }, -- style for keywords
      identifiers = {}, -- style for identifiers
      functions = { italic = true, bold = true }, -- style for functions
      variables = {}, -- style for variables
      booleans = { bold = true }, -- style for booleans
    },
  },
}
