local lspconfig = require("lspconfig")

lspconfig.ty.setup({
	root_dir = function(fname)
		return lspconfig.util.root_pattern("pyproject.toml", ".git")(fname)
	end,
})

lspconfig.ruff.setup({
	root_dir = function(fname)
		return lspconfig.util.root_pattern("pyproject.toml", "ruff.toml", ".ruff.toml", ".git")(fname)
	end,
	init_options = {
		settings = {
			fixAll = true,
		},
	},
})

lspconfig.lua_ls.setup({
	settings = {
		Lua = {
			runtime = { version = "LuaJIT" },
			diagnostics = { globals = { "vim" } },
			workspace = { checkThirdParty = false },
			telemetry = { enable = false },
		},
	},
})

vim.api.nvim_create_autocmd("LspAttach", {
	group = vim.api.nvim_create_augroup("lsp-attach", { clear = false }),
	callback = function(event)
		local client = vim.lsp.get_client_by_id(event.data.client_id)
		if client and client.supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint) then
			vim.lsp.inlay_hint.enable(false, { bufnr = event.buf })
		end
	end,
})

vim.lsp.enable({
	"ty",
	"ruff",
	"lua_ls",
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
