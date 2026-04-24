return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#131315',
				base01 = '#131315',
				base02 = '#878a92',
				base03 = '#878a92',
				base04 = '#dfe1ed',
				base05 = '#f8f9ff',
				base06 = '#f8f9ff',
				base07 = '#f8f9ff',
				base08 = '#ff9fb8',
				base09 = '#ff9fb8',
				base0A = '#d6def8',
				base0B = '#a5ffb4',
				base0C = '#ecf0ff',
				base0D = '#d6def8',
				base0E = '#e2e8ff',
				base0F = '#e2e8ff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#878a92',
				fg = '#f8f9ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#d6def8',
				fg = '#131315',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#878a92' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#ecf0ff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#e2e8ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#d6def8',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#d6def8',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#ecf0ff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#a5ffb4',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#dfe1ed' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#dfe1ed' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#878a92',
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
