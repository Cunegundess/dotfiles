return {
  'datsfilipe/vesper.nvim',
  config = function()
    require('vesper').setup {
      transparent = true,
      italics = {
        comments = true,
        keywords = true,
        functions = false,
        strings = true,
        variables = false,
      },
    }

    vim.api.nvim_set_hl(0, 'Normal', { bg = 'none' })
    vim.api.nvim_set_hl(0, 'NormalFloat', { bg = 'none' })
    vim.api.nvim_set_hl(0, 'FloatBorder', { bg = 'none' })
  end,
}
