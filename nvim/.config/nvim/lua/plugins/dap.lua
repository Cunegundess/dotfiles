return {
  'mfussenegger/nvim-dap',
  dependencies = {
    'rcarriga/nvim-dap-ui',
    'nvim-neotest/nvim-nio',
    'williamboman/mason.nvim',
    'jay-babu/mason-nvim-dap.nvim',
    'mfussenegger/nvim-dap-python',
    'theHamsta/nvim-dap-virtual-text',
    'jbyuki/one-small-step-for-vimkind',
  },
  keys = function(_, keys)
    local dap = require 'dap'
    local dapui = require 'dapui'
    return {
      {
        '<space>?',
        function()
          dapui.eval(nil, { enter = true })
        end,
        desc = 'Debug: Evaluate variable under cursor',
        mode = 'n',
      },

      { '<leader>bc', dap.continue, desc = 'Debug: Start/Continue' },
      { '<leader>bi', dap.step_into, desc = 'Debug: Step Into' },
      { '<leader>bo', dap.step_over, desc = 'Debug: Step Over' },
      { '<leader>bO', dap.step_out, desc = 'Debug: Step Out' },
      { '<leader>bb', dap.toggle_breakpoint, desc = 'Debug: Toggle Breakpoint' },
      {
        '<leader>bB',
        function()
          dap.set_breakpoint(vim.fn.input 'Breakpoint condition: ')
        end,
        desc = 'Debug: Set Conditional Breakpoint',
      },
      {
        'bt',
        dapui.toggle,
        desc = 'Debug: Toggle DAP UI (View last session result)',
      },
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
      controls = {
        element = 'repl',
        enabled = true,
        icons = {
          disconnect = '',
          pause = '',
          play = '',
          run_last = '',
          step_back = '',
          step_into = '',
          step_out = '',
          step_over = '',
          terminate = '',
        },
      },
      element_mappings = {},
      expand_lines = true,
      floating = {
        border = 'single',
        mappings = {
          close = { 'q', '<Esc>' },
        },
      },
      force_buffers = true,
      icons = {
        collapsed = '',
        current_frame = '',
        expanded = '',
      },
      layouts = {
        {

          elements = {
            {
              id = 'scopes',
              size = 0.60,
            },
            {
              id = 'breakpoints',
              size = 0.30,
            },
            {
              id = 'repl',
              size = 0.10,
            },
          },
          position = 'bottom',
          size = 5,
        },
      },
      mappings = {
        edit = 'e',
        expand = { '<CR>', '<2-LeftMouse>' },
        open = 'o',
        remove = 'd',
        repl = 'r',
        toggle = 't',
      },
      render = {
        indent = 1,
        max_value_lines = 100,
      },
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
      virt_text_pos = 'eol',

      -- experimental features:
      all_frames = false,
      virt_lines = false,
      virt_text_win_col = nil,

      display_callback = function(variable)
        local name = string.lower(variable.name)
        local value = string.lower(variable.value)
        if name:match 'secret' or name:match 'api' or value:match 'secret' or value:match 'api' then
          return '*****'
        end
        return ' ' .. variable.value
      end,
    }

    dap.listeners.after.event_initialized['dapui_config'] = dapui.open
    dap.listeners.before.event_terminated['dapui_config'] = dapui.close
    dap.listeners.before.event_exited['dapui_config'] = dapui.close

    require('dap-python').setup 'python3'

    dap.adapters.python = {
      type = 'server',
      host = '127.0.0.1',
      port = 5678,
    }

    dap.configurations.python = {
      {
        name = 'Docker',
        type = 'python',
        request = 'attach',
        connect = {
          host = '127.0.0.1',
          port = 5678,
        },
        pathMappings = {
          {
            localRoot = vim.fn.getcwd(),
            remoteRoot = '/app',
          },
        },
        justMyCode = true,
      },
    }
  end,
}
