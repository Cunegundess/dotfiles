vim.lsp.config("ruff", {})
vim.lsp.config("lua-ls", require("lsp.lua-ls"))
vim.lsp.config("basedpyright", require("lsp.basedpyright"))
vim.lsp.config("djls", require("lsp.djls"))

vim.lsp.enable({
	"basedpyright",
	"ruff",
	"lua_ls",
	"djls",
})

vim.diagnostic.config({
	virtual_lines = false,
	virtual_text = true,
	underline = true,
	update_in_insert = true,
	severity_sort = true,
	float = {
		border = "rounded",
		source = true,
	},
	signs = {
		text = {
			[vim.diagnostic.severity.ERROR] = "󰅚 ",
			[vim.diagnostic.severity.WARN] = "󰀪 ",
			[vim.diagnostic.severity.INFO] = "󰋽 ",
			[vim.diagnostic.severity.HINT] = "󰌶 ",
		},
	},
})

vim.lsp.inlay_hint.enable(false)
