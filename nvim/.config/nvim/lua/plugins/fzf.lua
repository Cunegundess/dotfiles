require("fzf-lua").setup({
	winopts = {
		height = 0.9,
		width = 0.9,
		border = "rounded",
		backdrop = 60,
	},
	preview = {
		border = "rounded",
		wrap = true,
		layout = "vertical",
		vertical = "up:60%",
	},
	previewers = {
		cat = {
			cmd = "cat",
			args = "-n",
		},
		bat = {
			cmd = "bat",
			args = "--color=always --style=numbers,changes",
		},
		builtin = {},
	},
	files = {
		fd_opts = [[--color=never --type f --hidden --follow
      --exclude .git
      --exclude node_modules
      --exclude venv
      --exclude .venv
      --exclude __pycache__
      --exclude media
      --exclude data
      --exclude staticfiles]],
	},
	grep = {
		rg_opts = [[--column --line-number --no-heading --color=always
      --smart-case
      --hidden
      --glob '!.git'
      --glob '!node_modules'
      --glob '!venv'
      --glob '!.venv'
      --glob '!__pycache__'
      --glob '!media'
      --glob '!data'
      --glob '!staticfiles']],
	},
})

require("fzf-lua").register_ui_select()

vim.defer_fn(function()
	require("theme").apply()
end, 0)

vim.keymap.set("n", "<leader>ff", require("fzf-lua").files, { desc = "[S]earch [F]iles" })
vim.keymap.set("n", "<leader>fw", require("fzf-lua").live_grep, { desc = "[S]earch current [W]ord" })
vim.keymap.set("n", "<leader>fg", require("fzf-lua").grep_visual, { desc = "[S]earch by [G]rep" })
vim.keymap.set("n", "<leader>fh", require("fzf-lua").help_tags, { desc = "[S]earch [H]elp" })
vim.keymap.set("n", "<leader>fk", require("fzf-lua").keymaps, { desc = "[S]earch [K]eymaps" })
vim.keymap.set("n", "<leader><leader>", require("fzf-lua").buffers, { desc = "[ ] Find existing buffers" })

vim.keymap.set("n", "<leader>ft", function()
	require("fzf-lua").colorschemes({
		actions = {
			["default"] = function(selected)
				local theme = selected[1]:gsub("%.vim$", "")
				require("theme").apply(theme)
			end,
		},
	})
end, { desc = "[S]earch [T]hemes" })

vim.keymap.set("n", "gd", require("fzf-lua").lsp_definitions, { desc = "[G]oto [D]efinition" })
vim.keymap.set("n", "gr", require("fzf-lua").lsp_references, { desc = "[G]oto [R]eferences" })
vim.keymap.set("n", "gI", require("fzf-lua").lsp_implementations, { desc = "[G]oto [I]mplementation" })
vim.keymap.set("n", "gD", require("fzf-lua").lsp_declarations, { desc = "[G]oto [D]eclaration" })
vim.keymap.set("n", "<leader>ca", require("fzf-lua").lsp_code_actions, { desc = "[C]ode [A]ctions" })
vim.keymap.set("n", "<leader>D", require("fzf-lua").lsp_typedefs, { desc = "Type [D]efinition" })
vim.keymap.set("n", "<leader>fd", require("fzf-lua").lsp_document_diagnostics, { desc = "[S]earch [D]iagnostics" })
vim.keymap.set("n", "<leader>fs", require("fzf-lua").lsp_document_symbols, { desc = "[S]earch [S]ymbols" })
vim.keymap.set("n", "<leader>gs", require("fzf-lua").git_status, { desc = "[G]it [S]tatus" })
vim.keymap.set("n", "<leader>gc", require("fzf-lua").git_commits, { desc = "[G]it [C]ommits" })
vim.keymap.set("n", "<leader>gb", require("fzf-lua").git_branches, { desc = "[G]it [B]ranches" })
vim.keymap.set("n", "<leader>gB", require("fzf-lua").git_blame, { desc = "[G]it [B]lame" })
