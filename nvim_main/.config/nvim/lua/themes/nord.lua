return {
  'arcticicestudio/nord-vim',
  config = function()
    vim.g.nord_cursor_line_number_background = 1
    vim.g.nord_uniform_status_lines = 1
    vim.g.nord_bold_vertical_split_line = 1
    vim.g.nord_uniform_diff_background = 1
    vim.g.nord_bold = 1
    vim.g.nord_italic_comments = 1
    vim.g.nord_italic = 1
    vim.g.nord_underline = 1

    vim.api.nvim_create_autocmd('ColorScheme', {
      pattern = 'nord',
      callback = function()
        local bg = '#161A1F'
        vim.api.nvim_set_hl(0, 'Normal', { bg = bg })
        vim.api.nvim_set_hl(0, 'NormalNC', { bg = bg })
        vim.api.nvim_set_hl(0, 'SignColumn', { bg = bg })
        vim.api.nvim_set_hl(0, 'NormalFloat', { bg = bg })

        vim.api.nvim_set_hl(0, 'FloatBorder', { bg = bg })
        vim.api.nvim_set_hl(0, 'Pmenu', { bg = bg })
        vim.api.nvim_set_hl(0, 'StatusLine', { bg = bg })
      end,
    })
  end,
}
