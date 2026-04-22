return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#131314',
				base01 = '#131314',
				base02 = '#7e8489',
				base03 = '#7e8489',
				base04 = '#d0d6dd',
				base05 = '#f8fbff',
				base06 = '#f8fbff',
				base07 = '#f8fbff',
				base08 = '#ff9fbc',
				base09 = '#ff9fbc',
				base0A = '#d3dde8',
				base0B = '#a4fdb0',
				base0C = '#f3f9ff',
				base0D = '#d3dde8',
				base0E = '#ecf5ff',
				base0F = '#ecf5ff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#7e8489',
				fg = '#f8fbff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#d3dde8',
				fg = '#131314',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#7e8489' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#f3f9ff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#ecf5ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#d3dde8',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#d3dde8',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#f3f9ff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#a4fdb0',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#d0d6dd' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#d0d6dd' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#7e8489',
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
