require("devdocs").setup({
	cmd = "DevDocs",
	ensure_installed = {
		"html",
		"http",
		"javascript",
		"lua~5.1",
		"python~3.12",
		"django~4.1",
	},
	previewer_cmd = "bat",
	cmd_args = { "--style=plain", "--color=always", "-p" },
	filetypes = {
		python = "python~3.12",
		lua = "lua~5.1",
		html = "html",
		javascript = "javascript",
	},
	open_mode = "split",
	wrap = true,
})

vim.keymap.set("n", "<leader>ho", "<cmd>DevDocs get<cr>", { desc = "DevDocs Open" })
vim.keymap.set("n", "<leader>hi", "<cmd>DevDocs install<cr>", { desc = "Devdocs Install" })
vim.keymap.set("n", "<leader>hd", "<cmd>DevDocs delete<cr>", { desc = "Devdocs Delete" })

vim.keymap.set("n", "<leader>hv", function()
	local devdocs = require("devdocs")
	local installedDocs = devdocs.GetInstalledDocs()

	vim.ui.select(installedDocs, {
		prompt = "Select doc > ",
	}, function(selected)
		if not selected then
			return
		end
		local docDir = devdocs.GetDocDir(selected)
		require("fzf-lua").files({
			cwd = docDir,
			prompt = selected .. " > ",
		})
	end)
end, { desc = "Browse Devdocs" })
