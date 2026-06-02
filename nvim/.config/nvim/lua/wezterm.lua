local M = {}

function M.is_wezterm()
	return vim.env.WEZTERM ~= nil
end

function M.set_tab_title(title)
	if not M.is_wezterm() then
		return
	end
	vim.fn.system(string.format('wezterm cli set-tab-title "%s" 2>/dev/null', title))
end

function M.update_tab_title()
	if not M.is_wezterm() then
		return
	end
	local project = vim.fn.fnamemodify(vim.fn.getcwd(), ":t")
	if project and project ~= "" then
		M.set_tab_title(project)
	end
end

function M.open_in_tab(cmd)
	if not M.is_wezterm() then
		return
	end
	if type(cmd) == "string" then
		cmd = { cmd }
	end
	vim.fn.system(
		string.format(
			'wezterm cli spawn --cwd "%s" %s 2>/dev/null',
			vim.fn.getcwd(),
			table.concat(vim.tbl_map(vim.fn.shellescape, cmd), " ")
		)
	)
end

function M.open_in_pane(direction)
	if not M.is_wezterm() then
		return
	end
	local pane_id = vim.env.WEZTERM_PANE
	if not pane_id then
		return
	end
	local flag = direction == "horizontal" and "--horizontal" or "--right"
	vim.fn.system(
		string.format('wezterm cli split-pane --pane-id %s %s --cwd "%s" 2>/dev/null', pane_id, flag, vim.fn.getcwd())
	)
	-- Muda pro novo pane
	vim.fn.system(string.format("wezterm cli activate-pane-direction --pane-id %s Right 2>/dev/null", pane_id))
end

function M.rename_workspace()
	if not M.is_wezterm() then
		return
	end
	vim.ui.input({ prompt = "WezTerm tab title: " }, function(title)
		if title then
			M.set_tab_title(title)
		end
	end)
end

-- Keymaps (só when WezTerm está presente)
if M.is_wezterm() then
	vim.keymap.set("n", "<leader>wn", M.rename_workspace, { desc = "Wezterm: Rename tab" })
	vim.keymap.set("n", "<leader>wt", function()
		M.open_in_tab({ "nvim", vim.fn.expand("%:p") })
	end, { desc = "Wezterm: Open file in new tab" })
	vim.keymap.set("n", "<leader>wv", function()
		M.open_in_pane("vertical")
	end, { desc = "Wezterm: New pane (vertical)" })
	vim.keymap.set("n", "<leader>wh", function()
		M.open_in_pane("horizontal")
	end, { desc = "Wezterm: New pane (horizontal)" })
end

-- Autocmd: atualiza título da aba ao entrar em projeto
local group = vim.api.nvim_create_augroup("wezterm_integration", { clear = true })
vim.api.nvim_create_autocmd({ "VimEnter", "DirChanged" }, {
	group = group,
	callback = function()
		M.update_tab_title()
	end,
})

-- Comando :WezTabTitle <nome>
vim.api.nvim_create_user_command("WezTabTitle", function(opts)
	M.set_tab_title(opts.args)
end, { nargs = 1 })

return M
