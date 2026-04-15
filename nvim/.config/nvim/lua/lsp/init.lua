local lspconfig = require("lspconfig")

lspconfig.lua_ls.setup({
  settings = {
    Lua = {
      runtime = { version = "LuaJIT" },
      diagnostics = { globals = { "vim" } },
      workspace = {
        library = vim.api.nvim_get_runtime_file("", true),
        checkThirdParty = false,
      },
    },
  },
})

lspconfig.ty.setup({
  cmd = { "ty", "server" },
})

lspconfig.ruff.setup({
  cmd = { "ruff", "server", "start" },
  settings = {
    ruff = {
      server = {
        settings = {
          preview = { pullRequests = true },
        },
      },
    },
  },
})

lspconfig.ts_ls.setup({})

vim.diagnostic.config({
  virtual_lines = false,
  virtual_text = false,
  underline = true,
  update_in_insert = true,
  severity_sort = true,
  float = {
    border = "rounded",
    source = true,
  },
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = "󰅚 ",
      [vim.diagnostic.severity.WARN] = "󰀪 ",
      [vim.diagnostic.severity.INFO] = "󰋽 ",
      [vim.diagnostic.severity.HINT] = "󰌶 ",
    },
  },
})

vim.lsp.inlay_hint.enable(false)
