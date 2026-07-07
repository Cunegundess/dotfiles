local ok_dap, dap = pcall(require, 'dap')
local ok_dapui, dapui = pcall(require, 'dapui')
if not (ok_dap and ok_dapui) then return end

local function python_path()
  local cwd = vim.fn.getcwd()
  if vim.fn.executable(cwd .. '/.venv/bin/python') == 1 then
    return cwd .. '/.venv/bin/python'
  end
  return 'python3'
end

dapui.setup()

local ok_pydap, _ = pcall(require, 'dap-python')
if ok_pydap then
  require('dap-python').setup('python3')
end

dap.configurations.python = {
  {
    type = 'python', request = 'launch', name = 'Django RunServer',
    program = vim.fn.getcwd() .. '/manage.py',
    args = { 'runserver', '--noreload' }, django = true, justMyCode = true,
    pythonPath = python_path,
  },
  {
    type = 'python', request = 'launch', name = 'Launch file',
    program = '${file}', pythonPath = python_path,
    console = 'integratedTerminal',
  },
  {
    type = 'python', request = 'launch', name = 'Launch file with args',
    program = '${file}',
    args = function()
      local args_string = vim.fn.input('Arguments: ')
      return vim.split(args_string, '%s+')
    end,
    pythonPath = python_path, console = 'integratedTerminal',
  },
  {
    type = 'python', request = 'attach', name = 'Attach remote',
    connect = function() return { host = '127.0.0.1', port = 5678 } end,
  },
}

dap.listeners.after.event_initialized['dapui_config'] = dapui.open
dap.listeners.before.event_terminated['dapui_config'] = dapui.close
dap.listeners.before.event_exited['dapui_config'] = dapui.close

local map = vim.keymap.set

map('n', '<leader>bc', dap.continue, { desc = 'Debug continue' })
map('n', '<leader>bi', dap.step_into, { desc = 'Debug step into' })
map('n', '<leader>bo', dap.step_over, { desc = 'Debug step over' })
map('n', '<leader>bO', dap.step_out, { desc = 'Debug step out' })
map('n', '<leader>bb', dap.toggle_breakpoint, { desc = 'Toggle breakpoint' })
map('n', 'bt', dapui.toggle, { desc = 'Debug: Toggle UI' })
map('n', '<leader>?', function() dapui.eval(nil, { enter = true }) end, { desc = 'Debug eval' })
