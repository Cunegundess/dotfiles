return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#141314',
				base01 = '#141314',
				base02 = '#85838d',
				base03 = '#85838d',
				base04 = '#dad7e4',
				base05 = '#faf8ff',
				base06 = '#faf8ff',
				base07 = '#faf8ff',
				base08 = '#ff9fb3',
				base09 = '#ff9fb3',
				base0A = '#dfdaef',
				base0B = '#a5ffb8',
				base0C = '#f6f2ff',
				base0D = '#dfdaef',
				base0E = '#f1ecff',
				base0F = '#f1ecff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#85838d',
				fg = '#faf8ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#dfdaef',
				fg = '#141314',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#85838d' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#f6f2ff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#f1ecff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#dfdaef',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#dfdaef',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#f6f2ff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#a5ffb8',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#dad7e4' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#dad7e4' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#85838d',
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
