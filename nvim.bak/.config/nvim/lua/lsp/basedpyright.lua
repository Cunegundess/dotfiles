local M = {}

function M.set_python_path(path)
	local clients = vim.lsp.get_clients({
		bufnr = vim.api.nvim_get_current_buf(),
		name = "basedpyright",
	})
	for _, client in ipairs(clients) do
		if client.settings then
			client.settings.python = vim.tbl_deep_extend(
				"force",
				client.settings.python or {},
				{ pythonPath = path }
			)
		else
			client.config.settings = vim.tbl_deep_extend(
				"force",
				client.config.settings,
				{ python = { pythonPath = path } }
			)
		end
		client:notify("workspace/didChangeConfiguration", { settings = nil })
	end
end

return {
	cmd = { "basedpyright-langserver", "--stdio" },
	filetypes = { "python" },
	root_markers = {
		"pyproject.toml",
		"setup.py",
		"setup.cfg",
		"requirements.txt",
		"Pipfile",
		"pyrightconfig.json",
		".git",
		"manage.py",
	},
	settings = {
		basedpyright = {
			analysis = {
				autoSearchPaths = true,
				autoImportCompletions = true,
				useLibraryCodeForTypes = true,
				diagnosticMode = "openFilesOnly",
				typeCheckingMode = "basic",
				inlayHints = {
					variableTypes = true,
					functionReturnTypes = true,
					paramTypes = true,
					callArgumentNames = true,
				},
			},
		},
	},
	on_attach = function(client, bufnr)
		vim.api.nvim_buf_create_user_command(bufnr, "LspPyrightOrganizeImports", function()
			local params = {
				command = "basedpyright.organizeimports",
				arguments = { vim.uri_from_bufnr(bufnr) },
			}
			client.request("workspace/executeCommand", params, nil, bufnr)
		end, { desc = "Organize Imports" })

		vim.api.nvim_buf_create_user_command(bufnr, "LspSetPythonPath", function(opts)
			M.set_python_path(opts.args)
		end, { desc = "Set python path for basedpyright", nargs = 1, complete = "file" })
	end,
}
