return {
  'ThePrimeagen/harpoon',
  dependencies = { 'nvim-lua/plenary.nvim' },
  config = function()
    require('harpoon').setup {
      menu = {
        width = vim.api.nvim_win_get_width(0) - 75,
      },
    }

    -- vim.keymap.set('n', '<leader>ha', require('harpoon.mark').add_file)
    -- vim.keymap.set('n', '<leader>ht', require('harpoon.ui').toggle_quick_menu)
    -- vim.keymap.set('n', '<leader>hn', require('harpoon.ui').nav_next)
    -- vim.keymap.set('n', '<leader>hp', require('harpoon.ui').nav_prev)

    require('telescope').load_extension 'harpoon'
  end,
}
