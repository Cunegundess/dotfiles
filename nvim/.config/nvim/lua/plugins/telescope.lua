return {
  -- Fuzzy Finder (files, lsp, etc)
  'nvim-telescope/telescope.nvim',
  event = 'VimEnter',
  branch = '0.1.x',
  dependencies = {
    'nvim-lua/plenary.nvim',
    {
      -- If encountering errors, see telescope-fzf-native README for installation instructions
      'nvim-telescope/telescope-fzf-native.nvim',
      -- `build` is used to run some command when the plugin is installed/updated.
      -- This is only run then, not every time Neovim starts up.
      build = 'make',
      -- `cond` is a condition used to determine whether this plugin should be
      -- installed and loaded.
      cond = function()
        return vim.fn.executable 'make' == 1
      end,
    },
    { 'nvim-telescope/telescope-ui-select.nvim' },
    -- Useful for getting pretty icons, but requires a Nerd Font.
    { 'nvim-tree/nvim-web-devicons', enabled = vim.g.have_nerd_font },
  },
  config = function()
    local telescope = require 'telescope'
    local builtin = require 'telescope.builtin'
    local keymap = vim.keymap.set

    telescope.setup {
      defaults = require('telescope.themes').get_ivy {
        file_ignore_patterns = {
          'node_modules',
          'git',
          'venv',
        },
        previewer = true,
        layout_config = {
          height = 0.4,
        },
      },
      pickers = {
        colorscheme = {
          enable_preview = true,
        },
        find_files = {
          hidden = true,
        },
      },
      extensions = {
        ['ui-select'] = {
          require('telescope.themes').get_ivy(),
        },
      },
    }

    -- Enable Telescope extensions if they are installed
    pcall(telescope.load_extension, 'fzf')
    pcall(telescope.load_extension, 'ui-select')
    pcall(telescope.load_extension, 'flutter')

    keymap('n', 'gd', builtin.lsp_definitions, { desc = '[G]oto [D]efinition' })
    keymap('n', 'gr', builtin.lsp_references, { desc = '[G]oto [R]eferences' })
    keymap('n', 'gI', builtin.lsp_implementations, { desc = '[G]oto [I]mplementation' })
    keymap('n', '<leader>D', builtin.lsp_type_definitions, { desc = 'Type [D]efinition' })
    keymap('n', '<leader>sd', builtin.diagnostics, { desc = '[S]earch [D]iagnostics' })
    keymap('n', '<leader>ss', builtin.lsp_document_symbols, { desc = '[S]earch [S]ymbols' })

    keymap('n', '<leader>gs', builtin.git_status, { desc = '[G]it [S]tatus' })
    keymap('n', '<leader>gc', builtin.git_commits, { desc = '[G]it [C]ommits' })
    keymap('n', '<leader>gb', builtin.git_branches, { desc = '[G]it [B]ranches' })

    keymap('n', '<leader>sf', builtin.find_files, { desc = '[T]elescope [F]ind Files' })
    keymap('n', '<leader>sg', builtin.live_grep, { desc = '[T]elescope [G]rep' })
    keymap('n', '<leader>sh', builtin.help_tags, { desc = '[T]elescope [H]elp' })
    keymap('n', '<leader>so', builtin.oldfiles, { desc = '[T]elescope [O]ld Files' })
    keymap('n', '<leader>st', builtin.colorscheme, { desc = '[T]elescope [T]heme' })
    keymap('n', '<leader>sk', builtin.keymaps, { desc = '[T]elescope [K]eymaps' })
    keymap('n', '<leader>sb', builtin.builtin, { desc = '[T]elescope [S]elect Telescope picker' })
    keymap('n', '<leader>sw', builtin.grep_string, { desc = '[T]elescope grep current [W]ord' })
    keymap('n', '<leader>sd', builtin.diagnostics, { desc = '[T]elescope [D]iagnostics' })
    keymap('n', '<leader>sr', builtin.resume, { desc = '[T]elescope [R]esume last picker' })
    keymap('n', '<leader>s.', builtin.oldfiles, { desc = '[T]elescope Recent Files ("." for repeat)' })
    keymap('n', '<leader><leader>', builtin.buffers, { desc = '[ ] Find existing buffers' })
    keymap('n', '<leader>t/', function()
      builtin.current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
        winblend = 10,
        previewer = false,
      })
    end, { desc = '[T]elescope [/] Fuzzily search in current buffer' })

    keymap('n', '<leader>tn', function()
      builtin.find_files { cwd = vim.fn.stdpath 'config' }
    end, { desc = '[T]elescope [N]eovim config files' })

    keymap('n', '<leader>t/', function()
      builtin.live_grep {
        grep_open_files = true,
        prompt_title = 'Live Grep in Open Files',
      }
    end, { desc = '[T]elescope [/] in Open Files' })
  end,
}
