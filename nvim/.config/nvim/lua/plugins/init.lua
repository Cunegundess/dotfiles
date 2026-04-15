vim.pack.add({
	-- Themes
	"catppuccin/nvim",
	"folke/tokyonight.nvim",
	"rose-pine/neovim",
	"rebelot/kanagawa.nvim",
	"shaunsingh/nord.nvim",
	"folke/twilight.nvim",
	"folke/zen-mode.nvim",
	"y9san9/y9nika.nvim",
	"datsfilipe/vesper.nvim",
	"vague-theme/vague.nvim",
	"projekt0n/github-nvim-theme",

	-- LSP & Completion
	"saghen/blink.cmp",
	"L3MON4D3/LuaSnip",
	"neovim/nvim-lspconfig",

	-- UI
	"echasnovski/mini.nvim",
	"echasnovski/mini.icons",
	"folke/which-key.nvim",

	-- File navigation
	"stevearc/oil.nvim",
	"nvim-lua/plenary.nvim",
	"ibhagwan/fzf-lua",

	-- Git
	"lewis6991/gitsigns.nvim",
	"NeogitOrg/neogit",

	-- Formatting
	"stevearc/conform.nvim",
	"williamboman/mason.nvim",
	"WhoIsSethDaniel/mason-tool-installer.nvim",

	-- Syntax
	"nvim-treesitter/nvim-treesitter",
	"nvim-treesitter/nvim-treesitter-textobjects",
	"nvim-treesitter/nvim-treesitter-context",

	-- Debug
	"mfussenegger/nvim-dap",
	"rcarriga/nvim-dap-ui",
	"nvim-neotest/nvim-nio",
	"mfussenegger/nvim-dap-python",

	-- IA
	"github/copilot.vim",

	-- Utils
	"esmuellert/codediff.nvim",
	"m00qek/baleia.nvim",
})

vim.defer_fn(function()
	pcall(require, "plugins.mason")
	pcall(require, "plugins.treesitter")
	pcall(require, "plugins.mini")
	pcall(require, "plugins.whichkey")
	pcall(require, "plugins.conform")
	pcall(require, "plugins.gitsigns")
	pcall(require, "plugins.fzf")
	pcall(require, "plugins.oil")
	pcall(require, "plugins.blink")

	local ok_dap, dap_config = pcall(require, "plugins.dap")
	if ok_dap and type(dap_config) == "function" then
		dap_config()
	end
end, 0)
