vim.g.mapleader = " "
vim.g.maplocalleader = " "

require("config")
require("keymaps")
require("autocmds")
require("lsp")

local function clone(repo)
	local name = repo:match("[^/]+$")
	local path = vim.fn.stdpath("data") .. "/site/pack/vendor/start/" .. name
	if vim.fn.isdirectory(path) == 0 then
		vim.fn.system({ "git", "clone", "--depth", "1", "https://github.com/" .. repo, path })
	end
end

local plugins = {
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
}

for _, plugin in ipairs(plugins) do
	clone(plugin)
end

require("plugins")
