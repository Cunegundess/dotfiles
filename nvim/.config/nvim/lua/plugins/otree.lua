local ok, otree = pcall(require, "Otree")
if not ok then
	return
end

otree.setup({
	win_size = 40,
	open_on_startup = false,
	open_on_left = true,
	use_default_keymaps = true,
	focus_on_enter = true,
	hijack_netrw = true,
	show_hidden = true,
	show_ignore = false,
	cursorline = true,
	git_signs = true,
	lsp_signs = true,
	oil = "float",
})
