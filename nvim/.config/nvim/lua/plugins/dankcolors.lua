return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#121414',
				base01 = '#121414',
				base02 = '#8b9396',
				base03 = '#8b9396',
				base04 = '#e4eff3',
				base05 = '#f8fdff',
				base06 = '#f8fdff',
				base07 = '#f8fdff',
				base08 = '#ff9fbe',
				base09 = '#ff9fbe',
				base0A = '#daf4ff',
				base0B = '#a5ffaf',
				base0C = '#ebf9ff',
				base0D = '#daf4ff',
				base0E = '#e0f6ff',
				base0F = '#e0f6ff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#8b9396',
				fg = '#f8fdff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#daf4ff',
				fg = '#121414',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#8b9396' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#ebf9ff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#e0f6ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#daf4ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#daf4ff',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#ebf9ff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#a5ffaf',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#e4eff3' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#e4eff3' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#8b9396',
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
