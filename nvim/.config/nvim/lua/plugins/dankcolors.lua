return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#131315',
				base01 = '#131315',
				base02 = '#8d9199',
				base03 = '#8d9199',
				base04 = '#e8edf7',
				base05 = '#f8faff',
				base06 = '#f8faff',
				base07 = '#f8faff',
				base08 = '#ff9fb9',
				base09 = '#ff9fb9',
				base0A = '#cdddff',
				base0B = '#a5ffb3',
				base0C = '#e4edff',
				base0D = '#cdddff',
				base0E = '#d6e3ff',
				base0F = '#d6e3ff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#8d9199',
				fg = '#f8faff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#cdddff',
				fg = '#131315',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#8d9199' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#e4edff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#d6e3ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#cdddff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#cdddff',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#e4edff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#a5ffb3',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#e8edf7' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#e8edf7' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#8d9199',
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
