return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#131314',
				base01 = '#131314',
				base02 = '#7b7c85',
				base03 = '#7b7c85',
				base04 = '#caccd7',
				base05 = '#f8f9ff',
				base06 = '#f8f9ff',
				base07 = '#f8f9ff',
				base08 = '#ff9fb8',
				base09 = '#ff9fb8',
				base0A = '#d9dae1',
				base0B = '#9ff6ad',
				base0C = '#f9fbff',
				base0D = '#d9dae1',
				base0E = '#f7f8ff',
				base0F = '#f7f8ff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#7b7c85',
				fg = '#f8f9ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#d9dae1',
				fg = '#131314',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#7b7c85' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#f9fbff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#f7f8ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#d9dae1',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#d9dae1',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#f9fbff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#9ff6ad',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#caccd7' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#caccd7' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#7b7c85',
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
