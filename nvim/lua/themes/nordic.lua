return {
  'AlexvZyl/nordic.nvim',
  lazy = false,
  priority = 1000,
  config = function()
    require('nordic').setup {
      bold_keywords = true,
      italic_comments = true,
      telescope = {
        -- Available styles: `classic`, `flat`.
        style = 'flat',
      },
    }
    -- require('nordic').load()
  end,
}
