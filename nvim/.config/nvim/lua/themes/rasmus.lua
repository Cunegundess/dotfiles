return {
  'kvrohit/rasmus.nvim',
  lazy = false,
  priority = 1000,
  config = function()
    vim.g.rasmus_italic_comments = false
    vim.g.rasmus_variant = 'monochrome' -- or dark
  end,
}
