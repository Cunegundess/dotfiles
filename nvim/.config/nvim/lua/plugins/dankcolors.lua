return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#121316',
				base01 = '#121316',
				base02 = '#9599a1',
				base03 = '#9599a1',
				base04 = '#eff3ff',
				base05 = '#f8faff',
				base06 = '#f8faff',
				base07 = '#f8faff',
				base08 = '#ff9fb9',
				base09 = '#ff9fb9',
				base0A = '#c1d2ff',
				base0B = '#a5ffb3',
				base0C = '#dee7ff',
				base0D = '#c1d2ff',
				base0E = '#ccdaff',
				base0F = '#ccdaff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#9599a1',
				fg = '#f8faff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#c1d2ff',
				fg = '#121316',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#9599a1' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#dee7ff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#ccdaff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#c1d2ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#c1d2ff',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#dee7ff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#a5ffb3',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#eff3ff' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#eff3ff' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#9599a1',
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
