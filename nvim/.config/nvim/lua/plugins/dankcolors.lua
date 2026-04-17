return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#131313',
				base01 = '#131313',
				base02 = '#78827e',
				base03 = '#78827e',
				base04 = '#c5d2cc',
				base05 = '#f8fffc',
				base06 = '#f8fffc',
				base07 = '#f8fffc',
				base08 = '#ffbc9f',
				base09 = '#ffbc9f',
				base0A = '#d6dcd9',
				base0B = '#9cf09e',
				base0C = '#fbfffd',
				base0D = '#d6dcd9',
				base0E = '#f9fffc',
				base0F = '#f9fffc',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#78827e',
				fg = '#f8fffc',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#d6dcd9',
				fg = '#131313',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#78827e' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#fbfffd', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#f9fffc',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#d6dcd9',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#d6dcd9',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#fbfffd',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#9cf09e',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#c5d2cc' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#c5d2cc' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#78827e',
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
