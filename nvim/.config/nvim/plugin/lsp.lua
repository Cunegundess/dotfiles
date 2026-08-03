local function jdtls_bundles()
  local mason = vim.fn.stdpath('data') .. '/mason/packages'
  local bundles = {}
  for _, jar in ipairs(vim.fn.glob(mason .. '/java-debug-adapter/extension/server/com.microsoft.java.debug.plugin-*.jar', false, true)) do
    table.insert(bundles, jar)
  end
  -- java-test 0.43.x is incompatible with jdtls 1.60.0 (Require-Bundle:
  -- org.eclipse.jdt.junit4.runtime was renamed to org.eclipse.jdt.junit.runtime),
  -- causing "Could not resolve module: com.microsoft.java.test.plugin". Re-enable
  -- only when the Mason java-test version matches the installed jdtls.
  -- for _, jar in ipairs(vim.fn.glob(mason .. '/java-test/extension/server/com.microsoft.java.test.plugin-*.jar', false, true)) do
  --   table.insert(bundles, jar)
  -- end
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
      import = { gradle = { enabled = true, wrapper = { enabled = true }, offline = { enabled = false }, annotationProcessing = { enabled = true } } },
      eclipse = { downloadSources = true },
      maven = { downloadSources = true },
      implementationsCodeLens = { enabled = true },
      referencesCodeLens = { enabled = true },
      signatureHelp = { enabled = true },
    },
  },
})

vim.lsp.config('kotlin_language_server', {
  cmd = { 'kotlin-language-server' },
  filetypes = { 'kotlin' },
  root_markers = {
    'settings.gradle.kts', 'settings.gradle', 'build.gradle.kts',
    'build.gradle', 'pom.xml', '.git',
  },
  settings = {
    kotlin = {
      languageServer = { transport = 'stdio' },
      diagnostics = { enabled = true },
      compiler = { jvm = { target = '17' } },
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
  'kotlin_language_server',
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
    local client = vim.lsp.get_client_by_id(ev.data.client_id)
    if client and client.server_capabilities.completionProvider then
      vim.lsp.completion.enable(true, client.id)
    end
    -- kotlin-language-server 1.3.x crashes on textDocument/documentHighlight
    -- (KotlinLSException / NoTopLevelDescriptorProvider) whenever it re-analyzes
    -- the buffer through a virtual file, flooding logs with error 500. Disable it.
    if client and client.name == 'kotlin_language_server' then
      client.server_capabilities.documentHighlightProvider = false
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

