local map = vim.keymap.set

map('n', '<leader>gs', '<cmd>G<CR>', { desc = 'Git status' })
map('n', '<leader>gc', '<cmd>G commit<CR>', { desc = 'Git commit' })
map('n', '<leader>gb', '<cmd>G branch<CR>', { desc = 'Git branches' })
map('n', '<leader>gd', '<cmd>G diff<CR>', { desc = 'Git diff' })
map('n', '<leader>gD', '<cmd>G diff -- %<CR>', { desc = 'Git diff buffer' })
map('n', '<leader>gl', '<cmd>G log<CR>', { desc = 'Git log' })
map('n', '<leader>gL', '<cmd>G log -- %<CR>', { desc = 'Git log buffer' })
map('n', '<leader>ga', '<cmd>G commit --amend<CR>', { desc = 'Git amend' })
map('n', '<leader>gw', '<cmd>G write<CR>', { desc = 'Git add' })
map('n', '<leader>gx', '<cmd>G read<CR>', { desc = 'Git checkout' })
map('n', '<leader>g_', '<cmd>G blame<CR>', { desc = 'Git blame' })
