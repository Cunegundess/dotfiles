vim.pack.add({
	-- Themes
	"https://github.com/catppuccin/nvim",
	"https://github.com/folke/tokyonight.nvim",
	"https://github.com/rose-pine/neovim",
	"https://github.com/rebelot/kanagawa.nvim",
	"https://github.com/shaunsingh/nord.nvim",
	"https://github.com/folke/twilight.nvim",
	"https://github.com/folke/zen-mode.nvim",
	"https://github.com/y9san9/y9nika.nvim",
	"https://github.com/datsfilipe/vesper.nvim",
	"https://github.com/vague-theme/vague.nvim",
	"https://github.com/projekt0n/github-nvim-theme",

	-- LSP & Completion
	"https://github.com/saghen/blink.cmp",
	"https://github.com/L3MON4D3/LuaSnip",
	"https://github.com/neovim/nvim-lspconfig",
	"https://github.com/maskudo/devdocs.nvim",

	-- UI
	"https://github.com/echasnovski/mini.nvim",
	"https://github.com/echasnovski/mini.icons",
	"https://github.com/folke/which-key.nvim",
	"https://github.com/folke/trouble.nvim",

	-- File navigation
	"https://github.com/stevearc/oil.nvim",
	"https://github.com/Eutrius/Otree.nvim",
	"https://github.com/nvim-lua/plenary.nvim",
	"https://github.com/ibhagwan/fzf-lua",

	-- Git
	"https://github.com/lewis6991/gitsigns.nvim",
	"https://github.com/NeogitOrg/neogit",

	-- Formatting
	"https://github.com/stevearc/conform.nvim",
	"https://github.com/williamboman/mason.nvim",
	"https://github.com/WhoIsSethDaniel/mason-tool-installer.nvim",
	"https://github.com/mason-org/mason-lspconfig.nvim",

	-- Syntax
	"https://github.com/nvim-treesitter/nvim-treesitter",
	"https://github.com/nvim-treesitter/nvim-treesitter-textobjects",
	"https://github.com/nvim-treesitter/nvim-treesitter-context",

	-- Django
	"https://github.com/mizisu/django.nvim",

	-- Debug
	"https://github.com/mfussenegger/nvim-dap",
	"https://github.com/rcarriga/nvim-dap-ui",
	"https://github.com/nvim-neotest/nvim-nio",
	"https://github.com/mfussenegger/nvim-dap-python",

	-- IA
	"https://github.com/github/copilot.vim",

	-- Android
	"https://github.com/iamironz/android-nvim-plugin",

	-- Utils
	"https://github.com/esmuellert/codediff.nvim",
	"https://github.com/m00qek/baleia.nvim",
	"https://github.com/christoomey/vim-tmux-navigator",
	"https://github.com/folke/snacks.nvim",
	"https://github.com/MeanderingProgrammer/render-markdown.nvim",
	"https://github.com/nvim-orgmode/orgmode",
	"https://github.com/akinsho/org-bullets.nvim",

	-- Dadbod UI
	"https://github.com/tpope/vim-dadbod",
	"https://github.com/kristijanhusak/vim-dadbod-ui",
	"https://github.com/kristijanhusak/vim-dadbod-completion",
})

vim.defer_fn(function()
	pcall(require, "plugins.mason")
	pcall(require, "plugins.blink")
	pcall(require, "plugins.treesitter")
	pcall(require, "plugins.mini")
	pcall(require, "plugins.whichkey")
	pcall(require, "plugins.conform")
	pcall(require, "plugins.gitsigns")
	pcall(require, "plugins.fzf")
	pcall(require, "plugins.oil")
	pcall(require, "plugins.otree")
	pcall(require, "plugins.devdocs")
	pcall(require, "plugins.django")
	pcall(require, "plugins.render-markdown")
	pcall(require, "plugins.android")
	pcall(require, "plugins.dadbod")
	pcall(require, "plugins.org")

	local ok_dap, dap_config = pcall(require, "plugins.dap")
	if ok_dap and type(dap_config) == "function" then
		dap_config()
	end
end, 0)
