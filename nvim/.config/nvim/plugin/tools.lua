-- Treesitter
local ok_ts, treesitter = pcall(require, 'nvim-treesitter.configs')
if ok_ts then
  treesitter.setup({
    ensure_installed = {
      'c', 'cpp', 'go', 'rust', 'bash', 'lua', 'vim', 'vimdoc',
      'html', 'css', 'javascript', 'typescript', 'tsx',
      'json', 'yaml', 'toml', 'markdown', 'markdown_inline',
      'python', 'htmldjango', 'java', 'kotlin', 'groovy',
    },
    sync_install = false,
    auto_install = true,
    highlight = { enable = true, additional_vim_regex_highlighting = true },
    indent = { enable = true },
    textobjects = {
      select = {
        enable = true,
        lookahead = true,
        keymaps = {
          ['af'] = '@function.outer',
          ['if'] = '@function.inner',
          ['ac'] = '@class.outer',
          ['ic'] = '@class.inner',
          ['ai'] = '@conditional.outer',
          ['ii'] = '@conditional.inner',
          ['al'] = '@loop.outer',
          ['il'] = '@loop.inner',
          ['am'] = '@call.outer',
          ['im'] = '@call.inner',
          ['ab'] = '@block.outer',
          ['ib'] = '@block.inner',
          ['aa'] = '@parameter.outer',
          ['ia'] = '@parameter.inner',
          ['a/'] = '@comment.outer',
          ['i/'] = '@comment.inner',
          ['as'] = '@statement.outer',
        },
      },
      move = {
        enable = true,
        set_jumps = true,
        goto_next_start = {
          [']f'] = '@function.outer',
          [']c'] = '@class.outer',
          [']i'] = '@conditional.outer',
          [']l'] = '@loop.outer',
          [']m'] = '@call.outer',
          [']a'] = '@parameter.inner',
          [']/'] = '@comment.outer',
          [']s'] = '@statement.outer',
        },
        goto_next_end = {
          [']F'] = '@function.outer',
          [']C'] = '@class.outer',
          [']I'] = '@conditional.outer',
          [']L'] = '@loop.outer',
          [']M'] = '@call.outer',
          [']A'] = '@parameter.inner',
        },
        goto_previous_start = {
          ['[f'] = '@function.outer',
          ['[c'] = '@class.outer',
          ['[i'] = '@conditional.outer',
          ['[l'] = '@loop.outer',
          ['[m'] = '@call.outer',
          ['[a'] = '@parameter.inner',
          ['[/'] = '@comment.outer',
          ['[s'] = '@statement.outer',
        },
        goto_previous_end = {
          ['[F'] = '@function.outer',
          ['[C'] = '@class.outer',
          ['[I'] = '@conditional.outer',
          ['[L'] = '@loop.outer',
          ['[M'] = '@call.outer',
          ['[A'] = '@parameter.inner',
        },
      },
    },
  })
end

-- fzf-lua
local ok_fzf, fzf = pcall(require, 'fzf-lua')
if ok_fzf then
  fzf.setup({
    winopts = {
      height = 0.9,
      width = 0.9,
      backdrop = 60,
      preview = { layout = 'vertical', vertical = 'down:70%' },
    },
    previewers = {
      cat = { cmd = 'cat', args = '-n' },
      bat = { cmd = 'bat', args = '--color=always --style=numbers,changes' },
    },
    files = {
      fd_opts = '--color=never --type f --hidden --follow '
        .. '--exclude .git --exclude node_modules --exclude venv '
        .. '--exclude .venv --exclude __pycache__ --exclude media '
        .. '--exclude data --exclude staticfiles',
    },
    grep = {
      rg_opts = '--column --line-number --no-heading --color=always '
        .. '--smart-case --hidden '
        .. "--glob '!.git' --glob '!node_modules' --glob '!venv' "
        .. "--glob '!.venv' --glob '!__pycache__' --glob '!media' "
        .. "--glob '!data' --glob '!staticfiles'",
    },
  })
  fzf.register_ui_select()
end

-- DAP
local ok_dap, dap = pcall(require, 'dap')
local ok_dapui, dapui = pcall(require, 'dapui')
if ok_dap and ok_dapui then
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
      type = 'python',
      request = 'launch',
      name = 'Django RunServer',
      program = vim.fn.getcwd() .. '/manage.py',
      args = { 'runserver', '--noreload' },
      django = true,
      justMyCode = true,
      pythonPath = python_path,
    },
    {
      type = 'python',
      request = 'launch',
      name = 'Launch file',
      program = '${file}',
      pythonPath = python_path,
      console = 'integratedTerminal',
    },
    {
      type = 'python',
      request = 'launch',
      name = 'Launch file with args',
      program = '${file}',
      args = function()
        local args_string = vim.fn.input('Arguments: ')
        return vim.split(args_string, '%s+')
      end,
      pythonPath = python_path,
      console = 'integratedTerminal',
    },
    {
      type = 'python',
      request = 'attach',
      name = 'Attach remote',
      connect = function()
        return { host = '127.0.0.1', port = 5678 }
      end,
    },
  }

  dap.listeners.after.event_initialized['dapui_config'] = dapui.open
  dap.listeners.before.event_terminated['dapui_config'] = dapui.close
  dap.listeners.before.event_exited['dapui_config'] = dapui.close
end

-- Dadbod
vim.g.db_ui_use_nerd_fonts = 1
vim.g.db_ui_table_helpers = {
  postgresql = {
    Count = 'select count(*) from "{table}"',
  },
}
vim.g.dbs = {
  { name = 'jcn-dev', url = 'postgres://postgres:jmalianca2023@localhost:5432/JCN' },
  { name = 'edp-dev', url = 'postgresql://DB_DEV_USER:DB_DEV_PASSWORD@localhost:5432/DB_DEV' },
  { name = 'alianca-dev-sqlite', url = 'sqlite:////home/lucas-proxion/Projects/alianca/apps/backend/db.sqlite3' },
  { name = 'alianca-dev-postgres', url = 'postgres://postgres:jmalianca2023@localhost:5432/alianca_rfid' },
  { name = 'nexus-dev-postgres', url = 'postgres://postgres:jmnexus2023@localhost:5432/nexus_rfid' },
}

-- Gitsigns
local ok_gs, gitsigns = pcall(require, 'gitsigns')
if ok_gs then
  gitsigns.setup({
    signs = {
      add = { text = '┃' },
      change = { text = '┃' },
      delete = { text = '_' },
      topdelete = { text = '‾' },
      changedelete = { text = '~' },
      untracked = { text = '┆' },
    },
    current_line_blame = true,
  })
end

-- which-key
local ok_wk, wk = pcall(require, 'which-key')
if ok_wk then
  wk.setup({
    preset = 'helix',
    delay = 0,
    plugins = {
      spelling = { enabled = true },
      presets = {
        operators = true, motions = true, text_objects = true,
        windows = true, nav = true, z = true, g = true,
      },
    },
  })
end

-- oil.nvim
local ok_oil, oil = pcall(require, 'oil')
if ok_oil then
  oil.setup({
    view_options = { show_hidden = true },
  })
end

-- trouble.nvim
local ok_trouble, _ = pcall(require, 'trouble')

-- render-markdown.nvim
local ok_md, render = pcall(require, 'render-markdown')
if ok_md then
  render.setup({
    enabled = function()
      return vim.bo.filetype == 'markdown'
    end,
    render = {
      markdown = {
        indicators = {
          headings = { icon = '' },
        },
      },
      latex = { render = true },
    },
  })
end
