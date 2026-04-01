if true then
  return {}
end

return {
  'nvim-telescope/telescope.nvim',
  event = 'VimEnter',
  branch = '0.1.x',
  dependencies = {
    { 'nvim-lua/plenary.nvim' },
    { 'nvim-tree/nvim-web-devicons', enabled = vim.g.have_nerd_font },
    { 'nvim-telescope/telescope-ui-select.nvim' },
    { 'nvim-telescope/telescope-dap.nvim' },
    {
      'nvim-telescope/telescope-live-grep-args.nvim',
      version = '^1.0.0',
    },
    {
      'nvim-telescope/telescope-fzf-native.nvim',
      build = 'make',
      cond = function()
        return vim.fn.executable 'make' == 1
      end,
    },
  },
  config = function()
    local telescope = require 'telescope'
    local builtin = require 'telescope.builtin'
    local keymap = vim.keymap.set

    telescope.setup {
      defaults = {
        vimgrep_arguments = {
          'rg',
          '--follow', -- Follow symbolic links
          '--hidden', -- Search for hidden files
          '--no-heading', -- Don't group matches by each file
          '--with-filename', -- Print the file path with the matched lines
          '--line-number', -- Show line numbers
          '--column', -- Show column numbers
          '--smart-case', -- Smart case search

          -- Exclude some patterns from search
          '--glob=!**/.git/*',
          '--glob=!**/.idea/*',
          '--glob=!**/build/*',
          '--glob=!**/dist/*',
          '--glob=!**/node_modules/*',
          '--glob=!**/yarn.lock',
          '--glob=!**/package-lock.json',
        },
        file_ignore_patterns = {
          'node_modules',
          'git',
          'venv',
          'data',
          'static',
          'staticfiles',
        },
        previewer = true,
        layout_config = {
          height = 0.9,
          width = 0.9,
        },
      },
      pickers = {
        colorscheme = {
          enable_preview = true,
        },
        find_files = {
          hidden = true,
          no_ignore = true,
          find_command = {
            'rg',
            '--files',
            '--hidden',
            '--glob=!**/.git/*',
            '--glob=!**/.idea/*',
            '--glob=!**/build/*',
            '--glob=!**/dist/*',
            '--glob=!**/node_modules/*',
            '--glob=!**/yarn.lock',
            '--glob=!**/package-lock.json',
          },
        },
      },
      extensions = {
        ['ui-select'] = {},
      },
    }

    pcall(telescope.load_extension, 'fzf')
    pcall(telescope.load_extension, 'ui-select')
    pcall(telescope.load_extension, 'dap')
    pcall(telescope.load_extension, 'live_grep_args')

    keymap('n', 'gd', builtin.lsp_definitions, { desc = '[G]oto [D]efinition' })
    keymap('n', 'gr', builtin.lsp_references, { desc = '[G]oto [R]eferences' })
    keymap('n', 'gI', builtin.lsp_implementations, { desc = '[G]oto [I]mplementation' })
    keymap('n', '<leader>D', builtin.lsp_type_definitions, { desc = 'Type [D]efinition' })
    keymap('n', '<leader>fd', builtin.diagnostics, { desc = '[S]earch [D]iagnostics' })
    keymap('n', '<leader>fs', builtin.lsp_document_symbols, { desc = '[S]earch [S]ymbols' })

    keymap('n', '<leader>gs', builtin.git_status, { desc = '[G]it [S]tatus' })
    keymap('n', '<leader>gc', builtin.git_commits, { desc = '[G]it [C]ommits' })
    keymap('n', '<leader>gb', builtin.git_branches, { desc = '[G]it [B]ranches' })

    keymap('n', '<leader>ff', builtin.find_files, { desc = '[T]elescope [F]ind Files' })
    keymap('n', '<leader>fg', builtin.live_grep, { desc = '[T]elescope [G]rep' })
    keymap('n', '<leader>fG', ":lua require('telescope').extensions.live_grep_args.live_grep_args()<CR>")
    keymap('n', '<leader>fh', builtin.help_tags, { desc = '[T]elescope [H]elp' })
    keymap('n', '<leader>fo', builtin.oldfiles, { desc = '[T]elescope [O]ld Files' })
    keymap('n', '<leader>ft', builtin.colorscheme, { desc = '[T]elescope [T]heme' })
    keymap('n', '<leader>fk', builtin.keymaps, { desc = '[T]elescope [K]eymaps' })
    keymap('n', '<leader>fb', builtin.builtin, { desc = '[T]elescope [S]elect Telescope picker' })
    keymap('n', '<leader>fw', builtin.grep_string, { desc = '[T]elescope grep current [W]ord' })
    keymap('n', '<leader>fd', builtin.diagnostics, { desc = '[T]elescope [D]iagnostics' })
    keymap('n', '<leader>fr', builtin.resume, { desc = '[T]elescope [R]esume last picker' })
    keymap('n', '<leader>f.', builtin.oldfiles, { desc = '[T]elescope Recent Files ("." for repeat)' })
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
