return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#131315',
				base01 = '#131315',
				base02 = '#868791',
				base03 = '#868791',
				base04 = '#ddddeb',
				base05 = '#f8f8ff',
				base06 = '#f8f8ff',
				base07 = '#f8f8ff',
				base08 = '#ff9fb7',
				base09 = '#ff9fb7',
				base0A = '#dbddf6',
				base0B = '#a5ffb5',
				base0C = '#f0f1ff',
				base0D = '#dbddf6',
				base0E = '#e7e9ff',
				base0F = '#e7e9ff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#868791',
				fg = '#f8f8ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#dbddf6',
				fg = '#131315',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#868791' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#f0f1ff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#e7e9ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#dbddf6',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#dbddf6',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#f0f1ff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#a5ffb5',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#ddddeb' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#ddddeb' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#868791',
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
