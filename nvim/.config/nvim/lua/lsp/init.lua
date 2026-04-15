vim.lsp.enable({
	"lua_ls",
	-- "basedpyright",
	"ty",
	"ruff",
	"ts_ls",
})

vim.diagnostic.config({
	virtual_lines = false,
	virtual_text = false,
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
