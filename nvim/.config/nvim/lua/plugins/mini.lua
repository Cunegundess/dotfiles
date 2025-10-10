return {
  'echasnovski/mini.nvim',
  version = false,
  config = function()
    require('mini.ai').setup { n_lines = 500 }
    require('mini.starter').setup()
    require('mini.surround').setup()
    require('mini.pairs').setup()
    require('mini.comment').setup()
    require('mini.indentscope').setup { delay = 50, symbol = '│' }
    require('mini.notify').setup()
    require('mini.diff').setup()
    require('mini.animate').setup()
    require('mini.fuzzy').setup()
    require('mini.statusline').setup { use_icons = vim.g.have_nerd_font }

    require('mini.files').setup {
      mappings = {
        close = 'q',
        go_in = '<CR>',
        go_out = '<ESC>',
        mark_goto = "'",
        mark_set = 'm',
        reset = '<BS>',
        reveal_cwd = '@',
        show_help = 'g?',
        synchronize = '=',
        trim_left = '<',
        trim_right = '>',
      },
      vim.keymap.set('n', '<leader>e', require('mini.files').open, { desc = 'Abrir explorador de arquivos (mini.files)' }),
    }

    require('mini.pick').setup {
      options = {
        use_cache = true,
      },
    }

    local pickers = {
      files = function()
        require('mini.pick').builtin.files()
      end,

      grep = function()
        require('mini.pick').builtin.grep_live()
      end,

      buffers = function()
        require('mini.pick').builtin.buffers()
      end,

      help = function()
        require('mini.pick').builtin.help()
      end,

      recent = function()
        require('mini.pick').builtin.oldfiles()
      end,
    }

    vim.keymap.set('n', '<leader>ff', pickers.files, { desc = '[F]ind [F]ile' })
    vim.keymap.set('n', '<leader>fg', pickers.grep, { desc = '[F]ind by [G]rep' })
    vim.keymap.set('n', '<leader>fb', pickers.buffers, { desc = '[F]ind [B]uffer' })
    vim.keymap.set('n', '<leader>fh', pickers.help, { desc = '[F]ind [H]elp' })
    vim.keymap.set('n', '<leader>fr', pickers.recent, { desc = '[F]ind [R]ecent Files' })

    require('mini.clue').setup {
      triggers = {
        { mode = 'n', keys = '<Leader>' },
        { mode = 'x', keys = '<Leader>' },
      },
      clues = {
        require('mini.clue').gen_clues.builtin_completion(),
        require('mini.clue').gen_clues.g(),
        require('mini.clue').gen_clues.marks(),
        require('mini.clue').gen_clues.registers(),
        require('mini.clue').gen_clues.windows(),
        require('mini.clue').gen_clues.z(),
        { mode = 'n', keys = '<leader>f', desc = '+find' },
        { mode = 'n', keys = '<leader>e', desc = '+explorer' },
      },
      window = {
        delay = 100,
      },
    }
  end,
}
