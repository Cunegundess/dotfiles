return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#141315',
				base01 = '#141315',
				base02 = '#868590',
				base03 = '#868590',
				base04 = '#dcdbe9',
				base05 = '#f9f8ff',
				base06 = '#f9f8ff',
				base07 = '#f9f8ff',
				base08 = '#ff9fb5',
				base09 = '#ff9fb5',
				base0A = '#dddbf4',
				base0B = '#a5ffb6',
				base0C = '#f2f1ff',
				base0D = '#dddbf4',
				base0E = '#ebeaff',
				base0F = '#ebeaff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#868590',
				fg = '#f9f8ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#dddbf4',
				fg = '#141315',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#868590' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#f2f1ff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#ebeaff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#dddbf4',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#dddbf4',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#f2f1ff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#a5ffb6',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#dcdbe9' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#dcdbe9' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#868590',
				italic = true
			})

			local current_file_path = vim.fn.stdpath("config") .. "/lua/plugins/dankcolors.lua"
			if not _G._matugen_theme_watcher then
				local uv = vim.uv or vim.loop
				_G._matugen_theme_watcher = uv.new_fs_event()
				_G._matugen_theme_watcher:start(current_file_path, {}, vim.schedule_wrap(function()
					local new_spec = dofile(current_file_path)
					if new_spec and new_spec[1] and new_spec[1].config then
						new_spec[1].config()
						print("Theme reload")
					end
				end))
			end
		end
	}
}
