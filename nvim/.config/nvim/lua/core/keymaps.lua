-- [[ Keymaps ]]
local keymap = vim.keymap.set

-- Code actions
keymap('n', '<leader>ca', vim.lsp.buf.code_action, { desc = '[C]ode [A]ctions' })

-- Netrw
keymap('n', '<leader>e', '<cmd>Lexplore!<CR>', { desc = 'Open Netrw' })

-- Clear highlights on search when pressing <Esc> in normal mode
keymap('n', '<Esc>', '<cmd>nohlsearch<CR>', { desc = 'Clear search highlights' })

-- Diagnostic keymaps
keymap('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostic [Q]uickfix list' })

-- Terminal mode exit
keymap('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })

-- Disable arrow keys in normal mode
keymap('n', '<left>', '<cmd>echo "Use h to move!!"<CR>', { desc = 'Disable Left Arrow' })
keymap('n', '<right>', '<cmd>echo "Use l to move!!"<CR>', { desc = 'Disable Right Arrow' })
keymap('n', '<up>', '<cmd>echo "Use k to move!!"<CR>', { desc = 'Disable Up Arrow' })
keymap('n', '<down>', '<cmd>echo "Use j to move!!"<CR>', { desc = 'Disable Down Arrow' })

-- Window navigation
keymap('n', '<C-h>', '<C-w><C-h>', { desc = 'Move focus to the left window' })
keymap('n', '<C-l>', '<C-w><C-l>', { desc = 'Move focus to the right window' })
keymap('n', '<C-j>', '<C-w><C-j>', { desc = 'Move focus to the lower window' })
keymap('n', '<C-k>', '<C-w><C-k>', { desc = 'Move focus to the upper window' })

-- Resize splits using Alt + hjkl
keymap('n', '<M-l>', '<cmd>vertical resize -2<CR>', { desc = 'Resize split left' })
keymap('n', '<M-h>', '<cmd>vertical resize +2<CR>', { desc = 'Resize split right' })
keymap('n', '<M-k>', '<cmd>resize +2<CR>', { desc = 'Resize split down' })
keymap('n', '<M-j>', '<cmd>resize -2<CR>', { desc = 'Resize split up' })

-- Move selected text up/down
keymap('v', 'J', ":m '>+1<CR>gv=gv", { desc = 'Move selection down' })
keymap('v', 'K', ":m '<-2<CR>gv=gv", { desc = 'Move selection up' })

-- Join lines and maintain cursor
keymap('n', 'J', 'mzJ`z', { desc = 'Join lines and keep cursor' })

-- Replace word under cursor
keymap('n', '<leader>r', [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]], { desc = 'Replace word under cursor' })
