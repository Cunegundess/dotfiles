vim.keymap.set("", "<leader>cf", function()
	require("conform").format({ async = true, lsp_fallback = true })
end, { desc = "[C]ode Format" })

require("conform").setup({
	notify_on_error = true,
	format_on_save = function(bufnr)
		local disable_filetypes = { python = true }
		return {
			timeout_ms = 500,
			lsp_fallback = not disable_filetypes[vim.bo[bufnr].filetype],
		}
	end,
	formatters_by_ft = {
		lua = { "stylua" },
		markdown = { "mdsf" },
	},
})
