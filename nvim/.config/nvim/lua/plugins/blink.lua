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
			Snippet = "�RD ",
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
		default = { "lsp", "path", "snippets", "buffer" },
	},
})
