vim.pack.add({
	-- Themes
	"https://github.com/catppuccin/nvim",
	"https://github.com/folke/tokyonight.nvim",
	"https://github.com/rose-pine/neovim",
	"https://github.com/rebelot/kanagawa.nvim",
	"https://github.com/shaunsingh/nord.nvim",
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
	"https://github.com/chipsenkbeil/org-roam.nvim",

	-- Dadbod UI
	"https://github.com/tpope/vim-dadbod",
	"https://github.com/kristijanhusak/vim-dadbod-ui",
	"https://github.com/kristijanhusak/vim-dadbod-completion",

	-- Fold
	"https://github.com/chrisgrieser/nvim-origami",
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
	pcall(require, "plugins.origami")

	local ok_dap, dap_config = pcall(require, "plugins.dap")
	if ok_dap and type(dap_config) == "function" then
		dap_config()
	end
end, 0)

require("kanagawa").setup({
	compile = false,
	undercurl = true,
	commentStyle = { italic = true },
	functionStyle = {},
	keywordStyle = { italic = true },
	statementStyle = { bold = true },
	typeStyle = { bold = true },
	transparent = false,
	dimInactive = true,
	terminalColors = true,
	colors = {
		palette = {},
		theme = { wave = {}, lotus = {}, dragon = {}, all = { ui = { bg_gutter = "none" } } },
	},
	overrides = function(colors)
		local theme = colors.theme
		local makeDiagnosticColor = function(color)
			local c = require("kanagawa.lib.color")
			return { fg = color, bg = c(color):blend(theme.ui.bg, 0.95):to_hex() }
		end
		return {
			NormalFloat = { bg = "none" },
			FloatBorder = { bg = "none" },
			FloatTitle = { bg = "none" },

			-- Save an hlgroup with dark background and dimmed foreground
			-- so that you can use it where your still want darker windows.
			-- E.g.: autocmd TermOpen * setlocal winhighlight=Normal:NormalDark
			NormalDark = { fg = theme.ui.fg_dim, bg = theme.ui.bg_m3 },

			-- Popular plugins that open floats will link to NormalFloat by default;
			-- set their background accordingly if you wish to keep them dark and borderless
			LazyNormal = { bg = theme.ui.bg_m3, fg = theme.ui.fg_dim },
			MasonNormal = { bg = theme.ui.bg_m3, fg = theme.ui.fg_dim },

			TelescopeTitle = { fg = theme.ui.special, bold = true },
			TelescopePromptNormal = { bg = theme.ui.bg_p1 },
			TelescopePromptBorder = { fg = theme.ui.bg_p1, bg = theme.ui.bg_p1 },
			TelescopeResultsNormal = { fg = theme.ui.fg_dim, bg = theme.ui.bg_m1 },
			TelescopeResultsBorder = { fg = theme.ui.bg_m1, bg = theme.ui.bg_m1 },
			TelescopePreviewNormal = { bg = theme.ui.bg_dim },
			TelescopePreviewBorder = { bg = theme.ui.bg_dim, fg = theme.ui.bg_dim },

			Pmenu = { fg = theme.ui.shade0, bg = theme.ui.bg_p1 }, -- add `blend = vim.o.pumblend` to enable transparency
			PmenuSel = { fg = "NONE", bg = theme.ui.bg_p2 },
			PmenuSbar = { bg = theme.ui.bg_m1 },
			PmenuThumb = { bg = theme.ui.bg_p2 },

			DiagnosticVirtualTextHint = makeDiagnosticColor(theme.diag.hint),
			DiagnosticVirtualTextInfo = makeDiagnosticColor(theme.diag.info),
			DiagnosticVirtualTextWarn = makeDiagnosticColor(theme.diag.warning),
			DiagnosticVirtualTextError = makeDiagnosticColor(theme.diag.error),
		}
	end,
	background = {
		dark = "dragon",
		light = "lotus",
	},
})

require("vesper").setup({
	transparent = false,
	italics = {
		comments = true,
		keywords = true,
		functions = false,
		strings = true,
		variables = false,
	},
})
