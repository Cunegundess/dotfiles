return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#131314',
				base01 = '#131314',
				base02 = '#7e8089',
				base03 = '#7e8089',
				base04 = '#d0d2dd',
				base05 = '#f8f9ff',
				base06 = '#f8f9ff',
				base07 = '#f8f9ff',
				base08 = '#ff9fb8',
				base09 = '#ff9fb8',
				base0A = '#d9dbe8',
				base0B = '#a4fdb3',
				base0C = '#f6f7ff',
				base0D = '#d9dbe8',
				base0E = '#f1f4ff',
				base0F = '#f1f4ff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#7e8089',
				fg = '#f8f9ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#d9dbe8',
				fg = '#131314',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#7e8089' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#f6f7ff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#f1f4ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#d9dbe8',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#d9dbe8',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#f6f7ff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#a4fdb3',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#d0d2dd' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#d0d2dd' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#7e8089',
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
