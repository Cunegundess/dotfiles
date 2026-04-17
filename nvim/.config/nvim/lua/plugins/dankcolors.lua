return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#141315',
				base01 = '#141315',
				base02 = '#8a8994',
				base03 = '#8a8994',
				base04 = '#e3e2f0',
				base05 = '#f9f8ff',
				base06 = '#f9f8ff',
				base07 = '#f9f8ff',
				base08 = '#ff9fb4',
				base09 = '#ff9fb4',
				base0A = '#dfdcfb',
				base0B = '#a5ffb7',
				base0C = '#efedff',
				base0D = '#dfdcfb',
				base0E = '#e7e4ff',
				base0F = '#e7e4ff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#8a8994',
				fg = '#f9f8ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#dfdcfb',
				fg = '#141315',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#8a8994' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#efedff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#e7e4ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#dfdcfb',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#dfdcfb',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#efedff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#a5ffb7',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#e3e2f0' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#e3e2f0' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#8a8994',
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
