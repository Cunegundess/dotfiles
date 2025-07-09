return { -- Fuzzy Finder (files, lsp, etc)
  'nvim-telescope/telescope.nvim',
  event = 'VimEnter',
  branch = '0.1.x',
  dependencies = {
    'nvim-lua/plenary.nvim',
    { -- If encountering errors, see telescope-fzf-native README for installation instructions
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
    local keymap = vim.keymap.set

    telescope.setup {
      defaults = {
        previewer = true,
        layout_strategy = 'flex',
        layout_config = {
          vertical = { width = 0.5 },
          horizontal = { width = 0.9, height = 0.7 },
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
          require('telescope.themes').get_dropdown(),
        },
      },
    }

    -- Enable Telescope extensions if they are installed
    pcall(telescope.load_extension, 'fzf')
    pcall(telescope.load_extension, 'ui-select')

    local builtin = require 'telescope.builtin'

    keymap('n', '<leader>tf', builtin.find_files, { desc = '[T]elescope [F]ind Files' })
    keymap('n', '<leader>tg', builtin.live_grep, { desc = '[T]elescope [G]rep' })
    keymap('n', '<leader>tB', builtin.buffers, { desc = '[T]elescope [B]uffers' })
    keymap('n', '<leader>th', builtin.help_tags, { desc = '[T]elescope [H]elp' })
    keymap('n', '<leader>to', builtin.oldfiles, { desc = '[T]elescope [O]ld Files' })
    keymap('n', '<leader>tt', builtin.colorscheme, { desc = '[T]elescope [T]heme' })
    keymap('n', '<leader>tk', builtin.keymaps, { desc = '[T]elescope [K]eymaps' })
    keymap('n', '<leader>ts', builtin.builtin, { desc = '[T]elescope [S]elect Telescope picker' })
    keymap('n', '<leader>tw', builtin.grep_string, { desc = '[T]elescope grep current [W]ord' })
    keymap('n', '<leader>td', builtin.diagnostics, { desc = '[T]elescope [D]iagnostics' })
    keymap('n', '<leader>tr', builtin.resume, { desc = '[T]elescope [R]esume last picker' })
    keymap('n', '<leader>t.', builtin.oldfiles, { desc = '[T]elescope Recent Files ("." for repeat)' })
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
