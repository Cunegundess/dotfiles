return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#141314',
				base01 = '#141314',
				base02 = '#86838e',
				base03 = '#86838e',
				base04 = '#dbd8e5',
				base05 = '#faf8ff',
				base06 = '#faf8ff',
				base07 = '#faf8ff',
				base08 = '#ff9fb3',
				base09 = '#ff9fb3',
				base0A = '#e0daf0',
				base0B = '#a5ffb8',
				base0C = '#f5f2ff',
				base0D = '#e0daf0',
				base0E = '#f0ebff',
				base0F = '#f0ebff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#86838e',
				fg = '#faf8ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#e0daf0',
				fg = '#141314',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#86838e' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#f5f2ff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#f0ebff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#e0daf0',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#e0daf0',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#f5f2ff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#a5ffb8',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#dbd8e5' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#dbd8e5' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#86838e',
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
