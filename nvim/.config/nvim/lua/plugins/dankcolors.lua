return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#141316',
				base01 = '#141316',
				base02 = '#93929d',
				base03 = '#93929d',
				base04 = '#f1efff',
				base05 = '#f9f8ff',
				base06 = '#f9f8ff',
				base07 = '#f9f8ff',
				base08 = '#ff9fb4',
				base09 = '#ff9fb4',
				base0A = '#d8d1ff',
				base0B = '#a5ffb7',
				base0C = '#eae6ff',
				base0D = '#d8d1ff',
				base0E = '#dfd9ff',
				base0F = '#dfd9ff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#93929d',
				fg = '#f9f8ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#d8d1ff',
				fg = '#141316',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#93929d' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#eae6ff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#dfd9ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#d8d1ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#d8d1ff',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#eae6ff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#a5ffb7',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#f1efff' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#f1efff' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#93929d',
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
