return {
	"rebelot/kanagawa.nvim",
	config = function()
		require("kanagawa").setup({
			undercurl = true,
			commentStyle = { italic = true },
			functionStyle = {},
			keywordStyle = { italic = true },
			statementStyle = { bold = true },
			typeStyle = {},
			transparent = true, -- do not set background color
			dimInactive = false, -- dim inactive window `:h hl-NormalNC`
			terminalColors = true, -- define vim.g.terminal_color_{0,17}
		})
		-- setup must be called before loading
		-- vim.cmd.colorscheme "kanagawa-wave"
	end,
}
