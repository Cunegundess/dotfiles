vim.keymap.set("", "<leader>cf", function()
	require("conform").format({ async = true, lsp_fallback = true })
end, { desc = "[C]ode Format" })

require("conform").setup({
	notify_on_error = true,
	format_on_save = function(bufnr)
		return {
			timeout_ms = 500,
			lsp_fallback = true,
		}
	end,
	formatters_by_ft = {
		python = { "ruff" },
		lua = { "stylua" },
		markdown = { "mdsf" },
	},
})
