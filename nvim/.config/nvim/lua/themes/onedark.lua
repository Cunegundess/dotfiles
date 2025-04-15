return {
  'navarasu/onedark.nvim',
  config = function()
    require('onedark').setup {
      style = 'warmer',
      -- Options are italic, bold, underline, none
      -- You can configure multiple style with comma separated, For e.g., keywords = 'italic,bold'
      code_style = {
        comments = 'italic',
        keywords = 'none',
        functions = 'bold',
        strings = 'none',
        variables = 'none',
      },
    }
  end,
}
