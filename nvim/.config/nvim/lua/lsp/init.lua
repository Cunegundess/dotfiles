vim.lsp.config('lua_ls', {
  cmd = { 'lua-language-server' },
  filetypes = { 'lua' },
  root_markers = {
    '.luarc.json', '.luarc.jsonc', '.luacheckrc',
    '.stylua.toml', 'stylua.toml', '.git',
  },
})

vim.lsp.config('basedpyright', {
  cmd = { 'basedpyright-langserver', '--stdio' },
  filetypes = { 'python' },
  root_markers = {
    'pyrightconfig.json', 'pyproject.toml', 'setup.py', 'setup.cfg',
    'requirements.txt', 'Pipfile', '.git',
  },
  settings = {
    basedpyright = {
      analysis = {
        autoSearchPaths = true,
        autoImportCompletions = true,
        typeCheckingMode = 'basic',
        inlayHints = {
          paramTypes = true,
          callArgumentNames = true,
        },
      },
    },
  },
})

vim.lsp.config('ruff', {
  cmd = { 'ruff', 'server' },
  filetypes = { 'python' },
  root_markers = { 'pyproject.toml', 'ruff.toml', '.ruff.toml', '.git' },
})

vim.lsp.config('ts_ls', {
  cmd = { 'typescript-language-server', '--stdio' },
  filetypes = {
    'javascript', 'javascriptreact', 'javascript.jsx',
    'typescript', 'typescriptreact', 'typescript.tsx',
  },
  root_markers = { 'package.json', 'tsconfig.json', '.git' },
})

vim.lsp.config('jdtls', {
  cmd = { 'jdtls' },
  filetypes = { 'java' },
  root_markers = { 'pom.xml', 'build.gradle', '.git' },
})

vim.lsp.config('gopls', {
  cmd = { 'gopls' },
  filetypes = { 'go', 'gomod', 'gowork', 'gotmpl' },
  root_markers = { 'go.mod', 'go.sum', '.git' },
})

vim.lsp.config('rust_analyzer', {
  cmd = { 'rust-analyzer' },
  filetypes = { 'rust' },
  root_markers = { 'Cargo.toml', 'rust-project.json', '.git' },
})

vim.api.nvim_create_user_command('LspInfo', function()
  local clients = vim.lsp.get_active_clients()
  if #clients == 0 then
    print 'No LSP clients attached'
    return
  end
  print('LSP clients (' .. #clients .. '):')
  for _, client in ipairs(clients) do
    print('  - ' .. client.name)
  end
end, {})

vim.api.nvim_create_user_command('LspInstall', function()
  local ok, mason = pcall(require, 'mason')
  if ok then
    vim.cmd 'MasonInstall basedpyright lua-language-server typescript-language-server ruff stylua'
  else
    print 'Mason not available. Install LSPs manually.'
  end
end, {})

vim.lsp.enable {
  'lua_ls',
  'basedpyright',
  'ruff',
  'ts_ls',
}

vim.lsp.inlay_hint.enable(true)

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
  },
}

vim.lsp.util.open_floating_preview = (function()
  local orig = vim.lsp.util.open_floating_preview
  return function(contents, syntax, opts, ...)
    opts = opts or {}
    opts.border = opts.border or 'rounded'
    return orig(contents, syntax, opts, ...)
  end
end)()

function _G.restart_lsp()
  local bufnr = vim.api.nvim_get_current_buf()
  local clients = vim.lsp.get_clients { bufnr = bufnr }
  for _, client in ipairs(clients) do
    vim.lsp.stop_client(client.id)
  end
  vim.defer_fn(function() vim.cmd 'edit' end, 100)
end
