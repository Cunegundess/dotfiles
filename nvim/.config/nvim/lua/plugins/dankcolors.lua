return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#141312',
				base01 = '#141312',
				base02 = '#828178',
				base03 = '#828178',
				base04 = '#d3d0c6',
				base05 = '#fffdf8',
				base06 = '#fffdf8',
				base07 = '#fffdf8',
				base08 = '#ffa99f',
				base09 = '#ffa99f',
				base0A = '#dddbd0',
				base0B = '#a7f19c',
				base0C = '#fffdf7',
				base0D = '#dddbd0',
				base0E = '#fffdf2',
				base0F = '#fffdf2',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#828178',
				fg = '#fffdf8',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#dddbd0',
				fg = '#141312',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#828178' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#fffdf7', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#fffdf2',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#dddbd0',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#dddbd0',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#fffdf7',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#a7f19c',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#d3d0c6' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#d3d0c6' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#828178',
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
