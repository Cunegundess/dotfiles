require("orgmode").setup({
	org_agenda_files = "~/Documentos/notes/**/*",
	org_default_notes_file = "~/Documentos/notes/tasks.org",
})

require("org-bullets").setup({
	symbols = {
		list = "•",
		headlines = { "◉", "○", "✸", "✿" },
	},
})

require("org-roam").setup({
	directory = "~/Documentos/notes",
})

-- Experimental LSP support
vim.lsp.enable("org")
