-- debug.lua
--
-- Shows how to use the DAP plugin to debug your code.
--
-- Primarily focused on configuring the debugger for Go, but can
-- be extended to other languages as well. That's why it's called
-- kickstart.nvim and not kitchen-sink.nvim ;)

return {
  'mfussenegger/nvim-dap',
  dependencies = {
    'rcarriga/nvim-dap-ui',
    'nvim-neotest/nvim-nio',
    'williamboman/mason.nvim',
    'jay-babu/mason-nvim-dap.nvim',
    'mfussenegger/nvim-dap-python',
    'theHamsta/nvim-dap-virtual-text',
  },
  keys = function(_, keys)
    local dap = require 'dap'
    local dapui = require 'dapui'
    return {
      -- Basic debugging keymaps, feel free to change to your liking!
      { '<F5>', dap.continue, desc = 'Debug: Start/Continue' },
      { '<F1>', dap.step_into, desc = 'Debug: Step Into' },
      { '<F2>', dap.step_over, desc = 'Debug: Step Over' },
      { '<F3>', dap.step_out, desc = 'Debug: Step Out' },
      { '<leader>b', dap.toggle_breakpoint, desc = 'Debug: Toggle Breakpoint' },
      {
        '<leader>B',
        function()
          dap.set_breakpoint(vim.fn.input 'Breakpoint condition: ')
        end,
        desc = 'Debug: Set Breakpoint',
      },
      -- Toggle to see last session result. Without this, you can't see session output in case of unhandled exception.
      { '<F7>', dapui.toggle, desc = 'Debug: See last session result.' },
      unpack(keys),
    }
  end,
  config = function()
    local dap = require 'dap'
    local dapui = require 'dapui'

    require('mason-nvim-dap').setup {
      automatic_installation = true,
      handlers = {},
      ensure_installed = {
        'debugpy',
      },
    }

    dapui.setup {
      icons = { expanded = '▾', collapsed = '▸', current_frame = '*' },
    }

    require('nvim-dap-virtual-text').setup {
      enabled = true,
      enabled_commands = true,
      highlight_changed_variables = true,
      highlight_new_as_changed = false,
      show_stop_reason = true,
      commented = false,
      only_first_definition = true,
      all_references = false,
      clear_on_continue = false,
      -- virt_text_pos = vim.fn.has 'nvim-0.10' == 1 and 'inline' or 'eol',
      virt_text_pos = 'eol',

      -- experimental features:
      all_frames = false, -- show virtual text for all stack frames not only current. Only works for debugpy on my machine.
      virt_lines = false, -- show virtual lines instead of virtual text (will flicker!)
      virt_text_win_col = nil,

      -- This just tries to mitigate the chance that I leak tokens here. Probably won't stop it from happening...
      display_callback = function(variable)
        local name = string.lower(variable.name)
        local value = string.lower(variable.value)
        if name:match 'secret' or name:match 'api' or value:match 'secret' or value:match 'api' then
          return '*****'
        end

        -- if #variable.value > 15 then
        --   return ' ' .. string.sub(variable.value, 1, 50) .. '... '
        -- end

        return ' ' .. variable.value
      end,
    }

    dap.listeners.after.event_initialized['dapui_config'] = dapui.open
    dap.listeners.before.event_terminated['dapui_config'] = dapui.close
    dap.listeners.before.event_exited['dapui_config'] = dapui.close

    require('dap-python').setup 'python3'

    dap.adapters.python = {
      type = 'server',
      host = 'localhost',
      port = 5678, -- porta onde o debugpy está escutando no container
    }

    dap.configurations.python = {
      {
        name = '🐳 Docker',
        type = 'python',
        request = 'attach',
        connect = {
          host = 'localhost',
          port = 5678,
        },
        pathMappings = {
          {
            localRoot = vim.fn.getcwd(),
            remoteRoot = '/app',
          },
        },
        justMyCode = false,
      },
    }

    -- Eval var under cursor
    vim.keymap.set('n', '<space>?', function()
      dapui.eval(nil, { enter = true })
    end)

    vim.keymap.set('n', '<leader>bc', dap.continue)
    vim.keymap.set('n', '<leader>bi', dap.step_into)
    vim.keymap.set('n', '<leader>bo', dap.step_over)
    vim.keymap.set('n', '<leader>bO', dap.step_out)
    vim.keymap.set('n', '<leader>bb', dap.step_back)
    vim.keymap.set('n', '<leader>br', dap.restart)
  end,
}
