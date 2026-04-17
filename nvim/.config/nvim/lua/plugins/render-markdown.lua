require("render-markdown").setup({
	enabled = function()
		return vim.bo.filetype == "markdown"
	end,
	pre_render_hooks = {
		function()
			vim.b.minianimate_disable = true
		end,
	},
	render = {
		markdown = {
			indicators = {
				headings = {
					icon = "",
				},
			},
		},
		latex = {
			render = false,
		},
	},
})

vim.api.nvim_create_autocmd({ "FileType" }, {
	pattern = "markdown",
	callback = function()
		vim.opt_local.conceallevel = 0
	end,
})