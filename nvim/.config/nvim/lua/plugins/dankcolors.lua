return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#111416',
				base01 = '#111416',
				base02 = '#90989c',
				base03 = '#90989c',
				base04 = '#edf7fd',
				base05 = '#f8fcff',
				base06 = '#f8fcff',
				base07 = '#f8fcff',
				base08 = '#ff9fbd',
				base09 = '#ff9fbd',
				base0A = '#abdfff',
				base0B = '#a5ffb0',
				base0C = '#d2eeff',
				base0D = '#abdfff',
				base0E = '#bae5ff',
				base0F = '#bae5ff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#90989c',
				fg = '#f8fcff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#abdfff',
				fg = '#111416',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#90989c' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#d2eeff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#bae5ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#abdfff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#abdfff',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#d2eeff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#a5ffb0',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#edf7fd' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#edf7fd' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#90989c',
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
