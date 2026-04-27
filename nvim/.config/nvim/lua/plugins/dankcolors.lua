return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#131315',
				base01 = '#131315',
				base02 = '#868690',
				base03 = '#868690',
				base04 = '#dcdcea',
				base05 = '#f8f8ff',
				base06 = '#f8f8ff',
				base07 = '#f8f8ff',
				base08 = '#ff9fb5',
				base09 = '#ff9fb5',
				base0A = '#dcdcf5',
				base0B = '#a5ffb6',
				base0C = '#f1f1ff',
				base0D = '#dcdcf5',
				base0E = '#eae9ff',
				base0F = '#eae9ff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#868690',
				fg = '#f8f8ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#dcdcf5',
				fg = '#131315',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#868690' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#f1f1ff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#eae9ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#dcdcf5',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#dcdcf5',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#f1f1ff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#a5ffb6',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#dcdcea' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#dcdcea' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#868690',
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
