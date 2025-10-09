-- [[ Keymaps ]]
local keymap = vim.keymap.set

-- Oil
-- vim.keymap.set('n', '<leader>e', function()
--   vim.g.oil_disable_git = true
--   vim.g.oil_disable_diagnostics = true
--   require('oil').open()
-- end, { desc = 'Open Oil' })

-- diagnostics (LSP)
keymap('n', 'gl', vim.diagnostic.open_float, opts)

-- LSP Code Actions
keymap('n', '<leader>ca', vim.lsp.buf.code_action, { desc = '[C]ode [A]ctions' })

-- Clear highlights on search when pressing <Esc> in normal mode
keymap('n', '<Esc>', '<cmd>nohlsearch<CR>')

-- Diagnostic keymaps
keymap('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostic [Q]uickfix list' })

-- Terminal mode exit
keymap('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })

-- Disable arrow keys in normal mode
keymap('n', '<left>', '<cmd>echo "Use h to move!!"<CR>')
keymap('n', '<right>', '<cmd>echo "Use l to move!!"<CR>')
keymap('n', '<up>', '<cmd>echo "Use k to move!!"<CR>')
keymap('n', '<down>', '<cmd>echo "Use j to move!!"<CR>')

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
keymap('v', 'J', ":m '>+1<CR>gv=gv")
keymap('v', 'K', ":m '<-2<CR>gv=gv")

-- Join lines and maintain cursor
keymap('n', 'J', 'mzJ`z')

-- Replace word under cursor
keymap('n', '<leader>r', [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])
