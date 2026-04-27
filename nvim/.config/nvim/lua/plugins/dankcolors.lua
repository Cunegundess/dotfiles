return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#121412',
				base01 = '#121412',
				base02 = '#7b857e',
				base03 = '#7b857e',
				base04 = '#cad7ce',
				base05 = '#f8fffa',
				base06 = '#f8fffa',
				base07 = '#f8fffa',
				base08 = '#ffb99f',
				base09 = '#ffb99f',
				base0A = '#c9e1d0',
				base0B = '#9ff6a0',
				base0C = '#f0fff4',
				base0D = '#c9e1d0',
				base0E = '#e8ffef',
				base0F = '#e8ffef',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#7b857e',
				fg = '#f8fffa',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#c9e1d0',
				fg = '#121412',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#7b857e' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#f0fff4', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#e8ffef',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#c9e1d0',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#c9e1d0',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#f0fff4',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#9ff6a0',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#cad7ce' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#cad7ce' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#7b857e',
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
