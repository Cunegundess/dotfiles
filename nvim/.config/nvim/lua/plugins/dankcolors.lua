return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#101417',
				base01 = '#101417',
				base02 = '#979fa4',
				base03 = '#979fa4',
				base04 = '#eff9ff',
				base05 = '#f8fcff',
				base06 = '#f8fcff',
				base07 = '#f8fcff',
				base08 = '#ff9cbc',
				base09 = '#ff9cbc',
				base0A = '#8fd8ff',
				base0B = '#a3ffad',
				base0C = '#c4eaff',
				base0D = '#8fd8ff',
				base0E = '#a3dfff',
				base0F = '#a3dfff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#979fa4',
				fg = '#f8fcff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#8fd8ff',
				fg = '#101417',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#979fa4' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#c4eaff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#a3dfff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#8fd8ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#8fd8ff',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#c4eaff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#a3ffad',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#eff9ff' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#eff9ff' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#979fa4',
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
