local dap = require 'dap'
local dapui = require 'dapui'

vim.keymap.set('n', '<leader>?', function()
  dapui.eval(nil, { enter = true })
end, { desc = 'Debug: Eval' })
vim.keymap.set('n', '<leader>bc', dap.continue, { desc = 'Debug: Start/Continue' })
vim.keymap.set('n', '<leader>bi', dap.step_into, { desc = 'Debug: Step Into' })
vim.keymap.set('n', '<leader>bo', dap.step_over, { desc = 'Debug: Step Over' })
vim.keymap.set('n', '<leader>bO', dap.step_out, { desc = 'Debug: Step Out' })
vim.keymap.set('n', '<leader>bb', dap.toggle_breakpoint, { desc = 'Debug: Toggle BP' })
vim.keymap.set('n', 'bt', dapui.toggle, { desc = 'Debug: Toggle UI' })

require('mason-nvim-dap').setup {
  automatic_installation = true,
  ensure_installed = { 'debugpy' },
}

dapui.setup()
require('dap-python').setup 'python3'

dap.listeners.after.event_initialized['dapui_config'] = dapui.open
dap.listeners.before.event_terminated['dapui_config'] = dapui.close
dap.listeners.before.event_exited['dapui_config'] = dapui.close
