require("devdocs").setup({
	cmd = "DevDocs",

	ensure_installed = {
		-- "go",
		"html",
		-- "dom",
		"http",
		-- "css",
		"javascript",
		-- "rust",
		-- some docs such as lua require version number along with the language name
		-- check `DevDocs install` to view the actual names of the docs
		"lua~5.1",
		"python~3.12",
		"django~4.1",
		-- "openjdk~21"
	},
})

vim.keymap.set("n", "<leader>ho", "<cmd>DevDocs get<cr>", { desc = "DevDocs Open" })
vim.keymap.set("n", "<leader>hi", "<cmd>DevDocs install<cr>", { desc = "Devdocs Install" })
vim.keymap.set("n", "<leader>hd", "<cmd>DevDocs delete<cr>", { desc = "Devdocs Delete" })
vim.keymap.set("n", "<leader>hv", function()
	local devdocs = require("devdocs")
	local installedDocs = devdocs.GetInstalledDocs()
	local Snacks = require("snacks")

	vim.ui.select(installedDocs, {}, function(selected)
		if not selected then
			return
		end
		local docDir = devdocs.GetDocDir(selected)
		-- prettify the filename as you wish
		Snacks.picker.files({ cwd = docDir })
	end)
end, { desc = "Install Devdocs" })
