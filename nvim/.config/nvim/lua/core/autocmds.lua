-- Alpha autocmd
local alpha_on_empty_group = vim.api.nvim_create_augroup('alpha_on_empty', { clear = true })
vim.api.nvim_create_autocmd('User', {
  pattern = 'BDeletePre *',
  group = alpha_on_empty_group,
  callback = function()
    local bufnr = vim.api.nvim_get_current_buf()
    local name = vim.api.nvim_buf_get_name(bufnr)

    if name == '' then
      vim.cmd [[:Alpha | bd#]]
    end
  end,
})
--
-- Yank highlight
vim.api.nvim_create_autocmd('TextYankPost', {
  desc = 'Highlight when yanking (copying) text',
  group = vim.api.nvim_create_augroup('kickstart-highlight-yank', { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})

-- Create autocommand which carries out the actual linting
local lint_augroup = vim.api.nvim_create_augroup('lint', { clear = true })
vim.api.nvim_create_autocmd({ 'BufEnter', 'BufWritePost', 'InsertLeave' }, {
  group = lint_augroup,
  callback = function()
    local lint = require 'lint'
    lint.try_lint()
  end,
})
