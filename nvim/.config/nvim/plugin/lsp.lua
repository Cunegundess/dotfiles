local function jdtls_bundles()
  local mason = vim.fn.stdpath('data') .. '/mason/packages'
  local bundles = {}
  for _, jar in ipairs(vim.fn.glob(mason .. '/java-debug-adapter/extension/server/com.microsoft.java.debug.plugin-*.jar', false, true)) do
    table.insert(bundles, jar)
  end
  for _, jar in ipairs(vim.fn.glob(mason .. '/java-test/extension/server/com.microsoft.java.test.plugin-*.jar', false, true)) do
    table.insert(bundles, jar)
  end
  return bundles
end

vim.lsp.config('ruff', {
  filetypes = { 'python' },
  root_markers = { 'pyproject.toml', 'setup.py', 'setup.cfg', 'requirements.txt', 'Pipfile', '.git' },
})

vim.lsp.config('jdtls', {
  cmd = function(dispatchers, config)
    local data = vim.fn.stdpath('cache') .. '/jdtls/workspace'
    if config.root_dir then
      data = data .. '/' .. vim.fn.fnamemodify(config.root_dir, ':p:h:t')
    end
    return vim.lsp.rpc.start({
      'jdtls', '-data', data,
      '--jvm-arg=-Xmx2G',
      '--jvm-arg=-XX:+UseParallelGC',
      '--jvm-arg=-XX:GCTimeRatio=4',
    }, dispatchers, { cwd = config.cmd_cwd })
  end,
  filetypes = { 'java' },
  root_markers = {
    { 'gradlew', 'settings.gradle.kts', 'settings.gradle', '.git' },
    { 'build.gradle.kts', 'build.gradle', 'pom.xml', 'build.xml' },
  },
  init_options = { bundles = jdtls_bundles() },
  settings = {
    java = {
      completion = { engine = 'ecj', lazyResolveTextEdit = { enabled = true } },
      import = { gradle = { enabled = true, wrapper = { enabled = true }, offline = { enabled = true }, annotationProcessing = { enabled = true } } },
      eclipse = { downloadSources = true },
      maven = { downloadSources = true },
      implementationsCodeLens = { enabled = true },
      referencesCodeLens = { enabled = true },
      signatureHelp = { enabled = true },
    },
  },
})

vim.lsp.config('lua-ls', {
  cmd = { 'lua-language-server' },
  filetypes = { 'lua' },
  root_markers = { '.luarc.json', '.luacheckrc', '.stylua.toml', '.git' },
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

vim.lsp.config('bash-language-server', {
  cmd = { 'bash-language-server', 'start' },
  filetypes = { 'sh', 'bash', 'zsh' },
  root_markers = { '.git' },
})

vim.lsp.enable({
  'basedpyright',
  'ruff',
  'lua-ls',
  'jdtls',
  'bash-language-server',
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

