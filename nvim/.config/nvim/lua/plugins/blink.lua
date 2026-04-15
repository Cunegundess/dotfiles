require("blink.cmp").setup({
	fuzzy = {
		implementation = "lua",
	},
	sources = {
		default = { "lsp", "path", "snippets", "buffer" },
		cmdline = { "buffer" },
	},
	snippets = {
		expand = function(snippet)
			require("luasnip").lsp_expand(snippet)
		end,
		active = function(params)
			return require("luasnip").locally_jumpable(-1)
		end,
		jump = function(direction)
			require("luasnip").jump(direction)
		end,
	},
	completion = {
		menu = {
			draw = {
				columns = {
					{ "kind_icon", "abbr", gap = 1 },
					{ "source_name", gap = 1 },
				},
			},
		},
	},
	signature = {
		enabled = true,
	},
})
