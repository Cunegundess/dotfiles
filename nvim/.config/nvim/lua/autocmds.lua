vim.api.nvim_create_autocmd("TextYankPost", {
	group = vim.api.nvim_create_augroup("highlight_yank", { clear = true }),
	pattern = "*",
	desc = "highlight selection on yank",
	callback = function()
		vim.highlight.on_yank({ timeout = 200, visual = true })
	end,
})

vim.api.nvim_create_autocmd("BufReadPost", {
	callback = function(args)
		local mark = vim.api.nvim_buf_get_mark(args.buf, '"')
		local line_count = vim.api.nvim_buf_line_count(args.buf)
		if mark[1] > 0 and mark[1] <= line_count then
			vim.api.nvim_win_set_cursor(0, mark)
			vim.schedule(function()
				vim.cmd("normal! zz")
			end)
		end
	end,
})

vim.api.nvim_create_autocmd("FileType", {
	pattern = "help",
	command = "wincmd L",
})

vim.api.nvim_create_autocmd("FileType", {
	group = vim.api.nvim_create_augroup("no_auto_comment", {}),
	callback = function()
		vim.opt_local.formatoptions:remove({ "c", "r", "o" })
	end,
})

vim.api.nvim_create_autocmd("BufRead", {
	group = vim.api.nvim_create_augroup("dotenv_ft", { clear = true }),
	pattern = { ".env", ".env.*" },
	callback = function()
		vim.bo.filetype = "dosini"
	end,
})

vim.api.nvim_create_autocmd({ "WinEnter", "BufEnter" }, {
	group = vim.api.nvim_create_augroup("active_cursorline", { clear = true }),
	callback = function()
		vim.opt_local.cursorline = true
	end,
})

vim.api.nvim_create_autocmd({ "WinLeave", "BufLeave" }, {
	group = "active_cursorline",
	callback = function()
		vim.opt_local.cursorline = false
	end,
})

vim.api.nvim_create_autocmd("CursorMoved", {
	group = vim.api.nvim_create_augroup("LspReferenceHighlight", { clear = true }),
	desc = "Highlight references under cursor",
	callback = function()
		if vim.fn.mode() ~= "i" then
			local clients = vim.lsp.get_clients({ bufnr = 0 })
			for _, client in ipairs(clients) do
				if client.server_capabilities.documentHighlightProvider then
					vim.lsp.buf.clear_references()
					vim.lsp.buf.document_highlight()
					return
				end
			end
		end
	end,
})

vim.api.nvim_create_autocmd("CursorMovedI", {
	group = "LspReferenceHighlight",
	desc = "Clear highlights when entering insert mode",
	callback = function()
		vim.lsp.buf.clear_references()
	end,
})

-- Django template filetype detection
vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
	group = vim.api.nvim_create_augroup("django_templates", { clear = true }),
	pattern = { "*/templates/*.html", "*/templates/*.txt", "*/templates/*.xml" },
	callback = function()
		vim.bo.filetype = "htmldjango"
	end,
})

-- local function make_transparent()
-- 	local groups = {
-- 		"Normal",
-- 		"NormalNC",
-- 		"NormalFloat",
-- 		"SignColumn",
-- 		"StatusLine",
-- 		"StatusLineNC",
-- 		"EndOfBuffer",
-- 	}
-- 	for _, group in ipairs(groups) do
-- 		vim.api.nvim_set_hl(0, group, { bg = "NONE", ctermbg = "NONE" })
-- 	end
-- 	vim.api.nvim_set_hl(0, "StatusLine", { reverse = false })
-- 	vim.api.nvim_set_hl(0, "StatusLineNC", { reverse = false })
-- end
--
-- vim.schedule(function()
-- 	make_transparent()
-- end)
--
-- vim.api.nvim_create_autocmd("ColorScheme", {
-- 	callback = make_transparent,
-- })
