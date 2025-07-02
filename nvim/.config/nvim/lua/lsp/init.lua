-- vim.lsp.config('pyright', require 'lsp.pyright')
vim.lsp.config('pylsp', require 'lsp.python-lsp-server')
vim.lsp.config('lua_ls', require 'lsp.lua-ls')

vim.lsp.enable { 'lua_ls', 'pylsp' }

vim.diagnostic.config {
  virtual_lines = false,
  -- virtual_text = true,
  underline = true,
  update_in_insert = true,
  severity_sort = true,
  float = {
    border = 'rounded',
    source = true,
  },
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = '󰅚 ',
      [vim.diagnostic.severity.WARN] = '󰀪 ',
      [vim.diagnostic.severity.INFO] = '󰋽 ',
      [vim.diagnostic.severity.HINT] = '󰌶 ',
    },
    numhl = {
      [vim.diagnostic.severity.ERROR] = 'ErrorMsg',
      [vim.diagnostic.severity.WARN] = 'WarningMsg',
    },
  },
}

local orig_util_open = vim.lsp.util.open_floating_preview

vim.lsp.util.open_floating_preview = function(contents, syntax, opts, ...)
  opts = opts or {}
  opts.border = opts.border or 'rounded'
  return orig_util_open(contents, syntax, opts, ...)
end
