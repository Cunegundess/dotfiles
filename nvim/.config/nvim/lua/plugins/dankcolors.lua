return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#131315',
				base01 = '#131315',
				base02 = '#8e8e9a',
				base03 = '#8e8e9a',
				base04 = '#e9eaf8',
				base05 = '#f8f8ff',
				base06 = '#f8f8ff',
				base07 = '#f8f8ff',
				base08 = '#ff9fb6',
				base09 = '#ff9fb6',
				base0A = '#e1e2ff',
				base0B = '#a5ffb5',
				base0C = '#efefff',
				base0D = '#e1e2ff',
				base0E = '#e6e7ff',
				base0F = '#e6e7ff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#8e8e9a',
				fg = '#f8f8ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#e1e2ff',
				fg = '#131315',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#8e8e9a' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#efefff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#e6e7ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#e1e2ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#e1e2ff',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#efefff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#a5ffb5',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#e9eaf8' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#e9eaf8' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#8e8e9a',
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
