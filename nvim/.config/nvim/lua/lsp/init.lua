-- vim.lsp.config('pyright', require 'lsp.pyright')
vim.lsp.config('basedpyright', require 'lsp.basedpyright')
vim.lsp.config('ruff', require 'lsp.ruff')
vim.lsp.config('tsserver', require 'lsp.tsserver')
-- vim.lsp.config('pylsp', require 'lsp.python-lsp-server')
vim.lsp.config('lua_ls', require 'lsp.lua-ls')
-- vim.lsp.config('jdtls', require 'lsp.jdtls')

vim.lsp.enable { 'lua_ls', 'basedpyright', 'ruff', 'jdtls', 'tsserver' }

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

-- Extras
function _G.restart_lsp(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  local clients = vim.lsp.get_clients { bufnr = bufnr }

  for _, client in ipairs(clients) do
    vim.lsp.stop_client(client.id)
  end

  vim.defer_fn(function()
    vim.cmd 'edit'
  end, 100)
end

function _G.lsp_info()
  local bufnr = vim.api.nvim_get_current_buf()
  local clients = vim.lsp.get_clients { bufnr = bufnr }

  print '═══════════════════════════════════'
  print '           LSP INFORMATION          '
  print '═══════════════════════════════════'
  print ''

  -- Basic info
  print('󰈙 Language client log: ' .. vim.lsp.get_log_path())
  print('󰈔 Detected filetype: ' .. vim.bo.filetype)
  print('󰈮 Buffer: ' .. bufnr)
  print('󰈔 Root directory: ' .. (vim.fn.getcwd() or 'N/A'))
  print ''

  if #clients == 0 then
    print('󰅚 No LSP clients attached to buffer ' .. bufnr)
    print ''
    print 'Possible reasons:'
    print('  • No language server installed for ' .. vim.bo.filetype)
    print '  • Language server not configured'
    print '  • Not in a project root directory'
    print '  • File type not recognized'
    return
  end

  print('󰒋 LSP clients attached to buffer ' .. bufnr .. ':')
  print '─────────────────────────────────'

  for i, client in ipairs(clients) do
    print(string.format('󰌘 Client %d: %s', i, client.name))
    print('  ID: ' .. client.id)
    print('  Root dir: ' .. (client.config.root_dir or 'Not set'))
    print('  Command: ' .. table.concat(client.config.cmd or {}, ' '))
    print('  Filetypes: ' .. table.concat(client.config.filetypes or {}, ', '))

    -- Server status
    if client.is_stopped() then
      print '  Status: 󰅚 Stopped'
    else
      print '  Status: 󰄬 Running'
    end

    -- Workspace folders
    if client.workspace_folders and #client.workspace_folders > 0 then
      print '  Workspace folders:'
      for _, folder in ipairs(client.workspace_folders) do
        print('    • ' .. folder.name)
      end
    end

    -- Attached buffers count
    local attached_buffers = {}
    for buf, _ in pairs(client.attached_buffers or {}) do
      table.insert(attached_buffers, buf)
    end
    print('  Attached buffers: ' .. #attached_buffers)

    -- Key capabilities
    local caps = client.server_capabilities
    local key_features = {}
    if caps.completionProvider then
      table.insert(key_features, 'completion')
    end
    if caps.hoverProvider then
      table.insert(key_features, 'hover')
    end
    if caps.definitionProvider then
      table.insert(key_features, 'definition')
    end
    if caps.documentFormattingProvider then
      table.insert(key_features, 'formatting')
    end
    if caps.codeActionProvider then
      table.insert(key_features, 'code_action')
    end

    if #key_features > 0 then
      print('  Key features: ' .. table.concat(key_features, ', '))
    end

    print ''
  end
end
