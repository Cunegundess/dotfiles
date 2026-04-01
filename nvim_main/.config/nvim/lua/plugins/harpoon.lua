return {
  'ThePrimeagen/harpoon',
  dependencies = { 'nvim-lua/plenary.nvim' },
  config = function()
    require('harpoon').setup {
      menu = {
        width = vim.api.nvim_win_get_width(0) - 75,
      },
    }

    local mark = require 'harpoon.mark'
    local ui = require 'harpoon.ui'

    -- Keymaps com descrições para o which-key
    vim.keymap.set('n', '<leader>ha', mark.add_file, { desc = 'Harpoon: Add file' })
    vim.keymap.set('n', '<leader>ht', ui.toggle_quick_menu, { desc = 'Harpoon: Toggle menu' })
    vim.keymap.set('n', '<leader>hn', ui.nav_next, { desc = 'Harpoon: Next file' })
    vim.keymap.set('n', '<leader>hp', ui.nav_prev, { desc = 'Harpoon: Previous file' })

    require('telescope').load_extension 'harpoon'
  end,
}
