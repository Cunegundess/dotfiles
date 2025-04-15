return {
  'kvrohit/rasmus.nvim',
  priority = 1000,
  init = function()
    -- Configure the appearance
    vim.g.rasmus_italic_comments = true
    vim.g.rasmus_italic_keywords = false
    vim.g.rasmus_italic_booleans = false
    vim.g.rasmus_italic_functions = true
    vim.g.rasmus_italic_variables = false
    vim.g.rasmus_bold_comments = false
    vim.g.rasmus_bold_keywords = false
    vim.g.rasmus_bold_booleans = false
    vim.g.rasmus_bold_functions = true
    vim.g.rasmus_bold_variables = false
    vim.g.rasmus_transparent = true
    vim.g.rasmus_variant = 'dark'

    -- vim.cmd.colorscheme 'rasmus'
  end,
}
