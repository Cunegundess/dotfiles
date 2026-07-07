local function load_mini(mod)
	pcall(require, mod)
end

load_mini("mini.ai")
load_mini("mini.surround")
load_mini("mini.pairs")
load_mini("mini.comment")
load_mini("mini.indentscope")
load_mini("mini.bracketed")
load_mini("mini.move")
load_mini("mini.splitjoin")
load_mini("mini.bufremove")
load_mini("mini.jump")
load_mini("mini.jump2d")
load_mini("mini.tabline")
load_mini("mini.trailspace")
load_mini("mini.icons")

require("mini.ai").setup({
	n_lines = 500,
	mappings = {
		around = "a",
		inside = "i",
		around_next = "an",
		inside_next = "in",
	},
})

require("mini.surround").setup()
require("mini.icons").setup()
require("mini.pairs").setup()
require("mini.comment").setup()
require("mini.indentscope").setup({
	symbol = "│",
	draw = { animation = require("mini.indentscope").gen_animation.none() },
})
require("mini.bracketed").setup({
	comment = { suffix = "" },
	file = { suffix = "" },
	indent = { suffix = "" },
	location = { suffix = "" },
})
require("mini.move").setup({
	lines = true,
	indent = true,
})
require("mini.splitjoin").setup()
require("mini.tabline").setup()
require("mini.trailspace").setup()

vim.keymap.set("n", "<leader>bd", function()
	require("mini.bufremove").delete(0, true)
end, { desc = "[B]uffer [D]elete" })

vim.keymap.set("n", "<leader>bw", function()
	require("mini.bufremove").wipeout(0, true)
end, { desc = "[B]uffer [W]ipeout" })
