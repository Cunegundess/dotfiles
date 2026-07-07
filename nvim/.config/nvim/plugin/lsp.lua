vim.lsp.config('ruff', {})

vim.lsp.config('lua-ls', {
  settings = {
    Lua = {
      diagnostics = { globals = { 'vim' } },
    },
  },
})

vim.lsp.config('basedpyright', {
  cmd = { 'basedpyright-langserver', '--stdio' },
  filetypes = { 'python' },
  root_markers = {
    'pyproject.toml', 'setup.py', 'setup.cfg', 'requirements.txt',
    'Pipfile', 'pyrightconfig.json', '.git', 'manage.py',
  },
  settings = {
    basedpyright = {
      analysis = {
        autoSearchPaths = true,
        autoImportCompletions = true,
        useLibraryCodeForTypes = true,
        diagnosticMode = 'openFilesOnly',
        typeCheckingMode = 'basic',
        inlayHints = {
          variableTypes = true,
          functionReturnTypes = true,
          paramTypes = true,
          callArgumentNames = true,
        },
      },
    },
  },
})

vim.lsp.enable({
  'basedpyright',
  'ruff',
  'lua-ls',
})

vim.diagnostic.config({
  virtual_lines = false,
  virtual_text = true,
  underline = true,
  update_in_insert = true,
  severity_sort = true,
  float = { border = 'rounded', source = true },
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = '󰅚 ',
      [vim.diagnostic.severity.WARN] = '󰀪 ',
      [vim.diagnostic.severity.INFO] = '󰋽 ',
      [vim.diagnostic.severity.HINT] = '󰌶 ',
    },
  },
})

vim.lsp.inlay_hint.enable(false)

vim.api.nvim_create_autocmd('LspAttach', {
  callback = function(ev)
    local map = function(mode, lhs, rhs, desc)
      vim.keymap.set(mode, lhs, rhs, { buffer = 0, desc = desc })
    end
    map('n', '<bs>', function()
      vim.diagnostic.config({ virtual_lines = not vim.diagnostic.config().virtual_lines })
      vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())
    end, 'Toggle verbose diagnostics and inlay_hints')
    map('n', 'gK', vim.diagnostic.open_float, 'Open diagnostic float')
    map('n', 'K', vim.lsp.buf.hover, 'Hover')
    map('n', '<leader>lr', vim.lsp.buf.rename, 'Rename')
    map('n', '<leader>la', vim.lsp.buf.code_action, 'Code actions')
    map('n', '<leader>li', vim.lsp.buf.hover, 'Hover info')
  end,
})

