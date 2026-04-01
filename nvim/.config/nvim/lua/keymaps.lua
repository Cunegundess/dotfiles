local keymap = vim.keymap.set

keymap("n", "<Esc>", "<cmd>nohlsearch<CR>", { desc = "Clear search highlights" })
keymap("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Open diagnostic [Q]uickfix" })
keymap("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })

keymap("n", "<left>", '<cmd>echo "Use h to move!!"<CR>', { desc = "Disable Left Arrow" })
keymap("n", "<right>", '<cmd>echo "Use l to move!!"<CR>', { desc = "Disable Right Arrow" })
keymap("n", "<up>", '<cmd>echo "Use k to move!!"<CR>', { desc = "Disable Up Arrow" })
keymap("n", "<down>", '<cmd>echo "Use j to move!!"<CR>', { desc = "Disable Down Arrow" })

keymap("n", "<C-h>", "<C-w><C-h>", { desc = "Move focus to the left window" })
keymap("n", "<C-l>", "<C-w><C-l>", { desc = "Move focus to the right window" })
keymap("n", "<C-j>", "<C-w><C-j>", { desc = "Move focus to the lower window" })
keymap("n", "<C-k>", "<C-w><C-k>", { desc = "Move focus to the upper window" })

keymap("n", "<M-l>", "<cmd>vertical resize -2<CR>", { desc = "Resize split left" })
keymap("n", "<M-h>", "<cmd>vertical resize +2<CR>", { desc = "Resize split right" })
keymap("n", "<M-k>", "<cmd>resize +2<CR>", { desc = "Resize split down" })
keymap("n", "<M-j>", "<cmd>resize -2<CR>", { desc = "Resize split up" })

keymap("n", "<leader>e", function()
	require("oil").open_float()
end, { desc = "Open Oil" })
keymap("n", "<leader>E", "<cmd>Explore!<CR>", { desc = "Open Netrw" })

keymap("n", "<leader>th", "<cmd>Twilight<CR>", { desc = "[T]heme [H]ighlight mode" })
keymap("n", "<leader>tz", "<cmd>ZenMode<CR>", { desc = "[T]heme [Z]en mode" })

-- vim.keymap.set({ 'n', 'x' }, 'J', ":m '>+1<CR>gv=gv", { desc = 'Move selection down' })
-- vim.keymap.set({ 'n', 'x' }, 'K', ":m '<-2<CR>gv=gv", { desc = 'Move selection up' })
-- keymap("n", "J", "mzJ`z", { desc = "Join lines and keep cursor" })
keymap("n", "<leader>r", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]], { desc = "Replace word under cursor" })
