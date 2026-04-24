return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#131313',
				base01 = '#131313',
				base02 = '#797f83',
				base03 = '#797f83',
				base04 = '#c7cfd4',
				base05 = '#f8fcff',
				base06 = '#f8fcff',
				base07 = '#f8fcff',
				base08 = '#ff9fbd',
				base09 = '#ff9fbd',
				base0A = '#d5dbde',
				base0B = '#9df2a7',
				base0C = '#f9fdff',
				base0D = '#d5dbde',
				base0E = '#f7fcff',
				base0F = '#f7fcff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#797f83',
				fg = '#f8fcff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#d5dbde',
				fg = '#131313',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#797f83' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#f9fdff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#f7fcff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#d5dbde',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#d5dbde',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#f9fdff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#9df2a7',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#c7cfd4' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#c7cfd4' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#797f83',
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
