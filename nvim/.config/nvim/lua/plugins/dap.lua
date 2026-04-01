return function()
  local ok_dap, dap = pcall(require, 'dap')
  if not ok_dap then return end
  
  local ok_dapui, dapui = pcall(require, 'dapui')
  if not ok_dapui then return end

  vim.keymap.set('n', '<leader>?', function()
    dapui.eval(nil, { enter = true })
  end, { desc = 'Debug: Eval' })
  vim.keymap.set('n', '<leader>bc', dap.continue, { desc = 'Debug: Start/Continue' })
  vim.keymap.set('n', '<leader>bi', dap.step_into, { desc = 'Debug: Step Into' })
  vim.keymap.set('n', '<leader>bo', dap.step_over, { desc = 'Debug: Step Over' })
  vim.keymap.set('n', '<leader>bO', dap.step_out, { desc = 'Debug: Step Out' })
  vim.keymap.set('n', '<leader>bb', dap.toggle_breakpoint, { desc = 'Debug: Toggle BP' })
  vim.keymap.set('n', 'bt', dapui.toggle, { desc = 'Debug: Toggle UI' })

  local ok_mason_dap, mason_dap = pcall(require, 'mason-nvim-dap')
  if ok_mason_dap then
    mason_dap.setup {
      automatic_installation = true,
      ensure_installed = { 'debugpy' },
    }
  end

  dapui.setup()
  
  local ok_dap_python, _ = pcall(require, 'dap-python')
  if ok_dap_python then
    require('dap-python').setup 'python3'
  end

  dap.listeners.after.event_initialized['dapui_config'] = dapui.open
  dap.listeners.before.event_terminated['dapui_config'] = dapui.close
  dap.listeners.before.event_exited['dapui_config'] = dapui.close
end
