return { -- Collection of various small independent plugins/modules
  'echasnovski/mini.nvim',
  config = function()
    -- Better Around/Inside textobjects
    --
    -- Examples:
    --  - va)  - [V]isually select [A]round [)]paren
    --  - yinq - [Y]ank [I]nside [N]ext [Q]uote
    --  - ci'  - [C]hange [I]nside [']quote
    require('mini.ai').setup { n_lines = 500 }

    -- Add/delete/replace surroundings (brackets, quotes, etc.)
    --
    -- - saiw) - [S]urround [A]dd [I]nner [W]ord [)]Paren
    -- - sd'   - [S]urround [D]elete [']quotes
    -- - sr)'  - [S]urround [R]eplace [)] [']
    require('mini.surround').setup()
    require('mini.pairs').setup()
    require('mini.animate').setup()
    require('mini.clue').setup()
    require('mini.diff').setup()
    require('mini.notify').setup()
    require('mini.indentscope').setup {
      delay = 50,
      symbol = '|',
    }

    require('mini.comment').setup {
      mappings = {
        comment = '<leader>cc',
        comment_line = '<leader>cc',
        comment_visual = '<leader>cc',
        textobject = '<leader>cc',
      },
    }

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

      vim.keymap.set('n', '<leader>e', '<cmd>lua MiniFiles.open()<CR>'),
    }

    require('mini.statusline').setup { use_icons = vim.g.have_nerd_font }
  end,
}
