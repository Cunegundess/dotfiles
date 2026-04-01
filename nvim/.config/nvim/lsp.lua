-- vim.lsp.config('pyright', require 'lsp.pyright')
vim.lsp.config('basedpyright', require 'lsp.basedpyright')
vim.lsp.config('ruff', require 'lsp.ruff')
vim.lsp.config('tsserver', require 'lsp.tsserver')
-- vim.lsp.config('pylsp', require 'lsp.python-lsp-server')
vim.lsp.config('lua_ls', require 'lsp.lua-ls')
-- vim.lsp.config('yamlls', require 'lsp.yamlls')
-- vim.lsp.config('marksman', require 'lsp.marksman')
-- vim.lsp.config('nginx_language_server', require 'lsp.nginx_language_server')
-- vim.lsp.config('rust_analyzer', require 'lsp.rust_analyzer')
vim.lsp.config('jdtls', require 'lsp.jdtls')
vim.lsp.config('kotlin-language-server', require 'lsp.kotlin-language-server')

vim.lsp.enable {
  'lua_ls',
  'basedpyright',
  'ruff',
  'tsserver',
  'jdtls',
  'kotlin-language-server',
  -- 'yamlls',
  -- 'marksman',
  -- 'nginx_language_server',
  -- 'rust_analyzer',
}

vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())

vim.diagnostic.config {
  virtual_lines = false,
  virtual_text = false,
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
