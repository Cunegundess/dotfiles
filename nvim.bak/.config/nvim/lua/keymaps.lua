local keymap = vim.keymap.set

keymap("n", "<Esc>", "<cmd>nohlsearch<CR>", { desc = "Clear search highlights" })
keymap("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Open diagnostic [Q]uickfix" })
keymap("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })

keymap("n", "<left>", '<cmd>echo "Use h to move!!"<CR>', { desc = "Disable Left Arrow" })
keymap("n", "<right>", '<cmd>echo "Use l to move!!"<CR>', { desc = "Disable Right Arrow" })
keymap("n", "<up>", '<cmd>echo "Use k to move!!"<CR>', { desc = "Disable Up Arrow" })
keymap("n", "<down>", '<cmd>echo "Use j to move!!"<CR>', { desc = "Disable Down Arrow" })

-- <C-h/j/k/l> é gerenciado pelo vim-tmux-navigator (WezTerm-aware)
-- As teclas <M-h/j/k/l> não funcionam no WezTerm (são interceptadas para navegação de panes)
-- keymap("n", "<M-l>", "<cmd>vertical resize -2<CR>", { desc = "Resize split left" })
-- keymap("n", "<M-h>", "<cmd>vertical resize +2<CR>", { desc = "Resize split right" })
-- keymap("n", "<M-k>", "<cmd>resize +2<CR>", { desc = "Resize split down" })
-- keymap("n", "<M-j>", "<cmd>resize -2<CR>", { desc = "Resize split up" })

keymap("n", "<leader>e", "<cmd>Otree<CR>", { desc = "Toggle file tree" })
keymap("n", "<leader>E", "<cmd>Explore!<CR>", { desc = "Open Netrw" })

keymap("n", "<leader>th", "<cmd>Twilight<CR>", { desc = "[T]heme [H]ighlight mode" })
keymap("n", "<leader>tz", "<cmd>ZenMode<CR>", { desc = "[T]heme [Z]en mode" })

-- vim.keymap.set({ 'n', 'x' }, 'J', ":m '>+1<CR>gv=gv", { desc = 'Move selection down' })
-- vim.keymap.set({ 'n', 'x' }, 'K', ":m '<-2<CR>gv=gv", { desc = 'Move selection up' })
-- keymap("n", "J", "mzJ`z", { desc = "Join lines and keep cursor" })
keymap("n", "<leader>r", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]], { desc = "Replace word under cursor" })

-- Django keymaps (django.nvim)
keymap("n", "<leader>dv", function()
	pcall(require, "django.views")
end, { desc = "Django: Open Views" })
keymap("n", "<leader>dm", function()
	pcall(require, "django.models")
end, { desc = "Django: Open Models" })
keymap("n", "<leader>du", function()
	pcall(require, "django.urls")
end, { desc = "Django: Open URLs" })
keymap("n", "<leader>ds", function()
	pcall(require, "django.shell")
end, { desc = "Django: Open Shell" })
