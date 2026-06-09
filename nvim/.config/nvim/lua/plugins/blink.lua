local ok, blink = pcall(require, "blink.cmp")
if not ok then
	return
end

blink.setup({
	completion = {
		menu = {
			border = "rounded",
			draw = {
				treesitter = { "lsp" },
			},
		},
		documentation = {
			window = { border = "rounded" },
		},
	},
	signature = {
		enabled = true,
		window = { border = "rounded" },
	},
	appearance = {
		kind_icons = {
			Text = "󰉿",
			Method = "󰆧",
			Function = "󰆧",
			Constructor = "󰆧",
			Field = "󰜢",
			Variable = "󰀫",
			Class = "󰠱",
			Interface = "󰒗",
			Module = "󰏗",
			Property = "󰜢",
			Unit = "󰑭",
			Value = "󰎠",
			Enum = "󰒡",
			Keyword = "󰌋",
			Snippet = "  ",
			Color = "󰏘",
			File = "󰈙",
			Reference = "󰂡",
			Folder = "󰉋",
			EnumMember = "󰒡",
			Constant = "󰏿",
			Struct = "󰙅",
			Event = "󰉿",
			Operator = "󰆕",
			TypeParameter = "󰅲",
		},
	},
	keymap = {
		preset = "default",
	},
	sources = {
		default = { "lsp", "path", "snippets", "buffer", "django" },
		providers = {
			django = {
				name = "Django",
				module = "django.completions.blink",
				async = true,
			},
		},
	},
})
