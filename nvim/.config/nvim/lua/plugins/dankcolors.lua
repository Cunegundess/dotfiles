return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#141315',
				base01 = '#141315',
				base02 = '#8d8c98',
				base03 = '#8d8c98',
				base04 = '#e7e6f5',
				base05 = '#f8f8ff',
				base06 = '#f8f8ff',
				base07 = '#f8f8ff',
				base08 = '#ff9fb5',
				base09 = '#ff9fb5',
				base0A = '#dcdaff',
				base0B = '#a5ffb6',
				base0C = '#ecebff',
				base0D = '#dcdaff',
				base0E = '#e2e1ff',
				base0F = '#e2e1ff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#8d8c98',
				fg = '#f8f8ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#dcdaff',
				fg = '#141315',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#8d8c98' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#ecebff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#e2e1ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#dcdaff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#dcdaff',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#ecebff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#a5ffb6',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#e7e6f5' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#e7e6f5' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#8d8c98',
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
