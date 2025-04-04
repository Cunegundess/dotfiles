return {
  'Yazeed1s/minimal.nvim',
  config = function()
    vim.g.minimal_italic_comments = true
    vim.g.minimal_italic_keywords = true
    vim.g.minimal_italic_booleans = true
    vim.g.minimal_italic_functions = true
    vim.g.minimal_italic_variables = true
    vim.g.minimal_transparent_background = false
    vim.cmd 'colorscheme minimal'
  end,
}
