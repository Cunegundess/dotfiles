-- Mason
local ok_mason, mason = pcall(require, 'mason')
if ok_mason then
  mason.setup()

  local ok_tool, tool_installer = pcall(require, 'mason-tool-installer')
  if ok_tool then
    tool_installer.setup({
      ensure_installed = {
        'basedpyright',
        'lua-language-server',
        'ruff',
        'stylua',
      },
      auto_update = false,
    })
  end
end

-- LSP configs
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

-- blink.cmp
local ok_blink, blink = pcall(require, 'blink.cmp')
if ok_blink then
  blink.setup({
    completion = {
      menu = { border = 'rounded', draw = { treesitter = { 'lsp' } } },
      documentation = { window = { border = 'rounded' } },
    },
    signature = { enabled = true, window = { border = 'rounded' } },
    appearance = {
      kind_icons = {
        Text = '󰉿', Method = '󰆧', Function = '󰆧', Constructor = '󰆧',
        Field = '󰜢', Variable = '󰀫', Class = '󰠱', Interface = '󰒗',
        Module = '󰏗', Property = '󰜢', Unit = '󰑭', Value = '󰎠',
        Enum = '󰒡', Keyword = '󰌋', Snippet = '', Color = '󰏘',
        File = '󰈙', Reference = '󰂡', Folder = '󰉋', EnumMember = '󰒡',
        Constant = '󰏿', Struct = '󰙅', Event = '󰉿', Operator = '󰆕',
        TypeParameter = '󰅲',
      },
    },
    sources = { default = { 'lsp', 'path', 'snippets', 'buffer' } },
  })
end

-- conform (formatting)
local ok_conf, conform = pcall(require, 'conform')
if ok_conf then
  conform.setup({
    notify_on_error = true,
    format_on_save = function()
      return { timeout_ms = 500, lsp_fallback = true }
    end,
    formatters_by_ft = {
      python = { 'ruff' },
      lua = { 'stylua' },
    },
  })
end
