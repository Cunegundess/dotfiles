return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#131313',
				base01 = '#131313',
				base02 = '#798083',
				base03 = '#798083',
				base04 = '#c7d0d4',
				base05 = '#f8fdff',
				base06 = '#f8fdff',
				base07 = '#f8fdff',
				base08 = '#ff9fbf',
				base09 = '#ff9fbf',
				base0A = '#d4dcde',
				base0B = '#9df2a6',
				base0C = '#f9fdff',
				base0D = '#d4dcde',
				base0E = '#f6fdff',
				base0F = '#f6fdff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#798083',
				fg = '#f8fdff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#d4dcde',
				fg = '#131313',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#798083' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#f9fdff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#f6fdff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#d4dcde',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#d4dcde',
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
				fg = '#9df2a6',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#c7d0d4' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#c7d0d4' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#798083',
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
