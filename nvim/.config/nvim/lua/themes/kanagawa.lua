return {
  'rebelot/kanagawa.nvim',
  config = function()
    require('kanagawa').setup {
      commentStyle = { italic = true },
      functionStyle = {},
      keywordStyle = { italic = true },
      statementStyle = { bold = true },
      typeStyle = {},
    }
  end,
}
