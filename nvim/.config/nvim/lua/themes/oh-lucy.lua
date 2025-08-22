return {
  'Yazeed1s/oh-lucy.nvim',
  lazy = false,
  priority = 1000,
  config = function()
    vim.g.oh_lucy_italic_comments = false
    vim.g.oh_lucy_italic_keywords = false
    vim.g.oh_lucy_italic_variables = false
    vim.g.oh_lucy_transparent_background = true
  end,
}
