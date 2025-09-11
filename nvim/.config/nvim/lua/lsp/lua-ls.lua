return {
  -- cmd = { 'lua-language-server' },
  cmd = { '/run/current-system/sw/bin/lua-language-server' },

  filetypes = { 'lua' },
  root_markers = { '.luarc.json', '.luarc.jsonc', '.git' },
  settings = {
    Lua = {
      completion = { callSnippet = 'Replace' },
      diagnostics = { disable = { 'undefined-global' } },
    },
  },
}
