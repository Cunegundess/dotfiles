local keymap = vim.keymap.set

keymap('n', '<C-w>s', '<C-w>v', { desc = 'Split vertical (lado a lado)' })
keymap('n', '<C-w>v', '<C-w>s', { desc = 'Split horizontal (cima/baixo)' })

vim.cmd([[cabbrev split vsplit]])
vim.cmd([[cabbrev new vnew]])
vim.cmd([[cabbrev h vert h]])
vim.cmd([[cabbrev he vert he]])
vim.cmd([[cabbrev hel vert hel]])
vim.cmd([[cabbrev help vert help]])

keymap('n', '<Esc>', '<cmd>nohlsearch<CR>', { desc = 'Clear search highlights' })
keymap('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostic quickfix' })
keymap('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })
keymap('t', '<C-[>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })

keymap('n', '<left>', '<cmd>echo "Use h to move!!"<CR>')
keymap('n', '<right>', '<cmd>echo "Use l to move!!"<CR>')
keymap('n', '<up>', '<cmd>echo "Use k to move!!"<CR>')
keymap('n', '<down>', '<cmd>echo "Use j to move!!"<CR>')

keymap('n', '<leader>e', '<cmd>Oil<CR>', { desc = 'Toggle file tree' })
keymap('n', '<leader>E', '<cmd>Explore!<CR>', { desc = 'Open Netrw' })

keymap('n', '<leader>r', [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]], { desc = 'Replace word under cursor' })
