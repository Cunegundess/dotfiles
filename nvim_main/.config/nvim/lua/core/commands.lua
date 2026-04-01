vim.api.nvim_create_user_command('LspRestart', function()
  restart_lsp()
end, {})

vim.api.nvim_create_user_command('LspInfo', function()
  lsp_info()
end, {})
