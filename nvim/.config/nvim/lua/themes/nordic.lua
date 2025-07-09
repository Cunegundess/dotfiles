return {
  'AlexvZyl/nordic.nvim',
  lazy = false,
  priority = 1000,
  config = function()
    require('nordic').setup {
      bold_keywords = false,
      italic_comments = false,
      bright_border = false,
      -- Reduce the overall amount of blue in the theme (diverges from base Nord).
      reduced_blue = true,
      transparent = {
        bg = true,
        float = true,
      },
      cursorline = {
        bold = false,
        bold_number = true,
        -- Available styles: 'dark', 'light'.
        theme = 'dark',
        -- Blending the cursorline bg with the buffer bg.
        blend = 0.85,
      },
      noice = {
        -- Available styles: `classic`, `flat`.
        style = 'classic',
      },
      telescope = {
        -- Available styles: `classic`, `flat`.
        style = 'flat',
      },
    }
  end,
}
