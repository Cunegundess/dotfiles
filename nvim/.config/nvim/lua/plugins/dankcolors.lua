return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#121316',
				base01 = '#121316',
				base02 = '#959aa1',
				base03 = '#959aa1',
				base04 = '#eff6ff',
				base05 = '#f8fbff',
				base06 = '#f8fbff',
				base07 = '#f8fbff',
				base08 = '#ff9fbb',
				base09 = '#ff9fbb',
				base0A = '#b8d5ff',
				base0B = '#a5ffb2',
				base0C = '#d9e9ff',
				base0D = '#b8d5ff',
				base0E = '#c4ddff',
				base0F = '#c4ddff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#959aa1',
				fg = '#f8fbff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#b8d5ff',
				fg = '#121316',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#959aa1' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#d9e9ff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#c4ddff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#b8d5ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#b8d5ff',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#d9e9ff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#a5ffb2',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#eff6ff' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#eff6ff' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#959aa1',
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
