return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#121414',
				base01 = '#121414',
				base02 = '#7e8889',
				base03 = '#7e8889',
				base04 = '#d0dcdd',
				base05 = '#f8feff',
				base06 = '#f8feff',
				base07 = '#f8feff',
				base08 = '#ff9fc0',
				base09 = '#ff9fc0',
				base0A = '#bbe3e8',
				base0B = '#a4fdac',
				base0C = '#e4fcff',
				base0D = '#bbe3e8',
				base0E = '#d6faff',
				base0F = '#d6faff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#7e8889',
				fg = '#f8feff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#bbe3e8',
				fg = '#121414',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#7e8889' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#e4fcff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#d6faff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#bbe3e8',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#bbe3e8',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#e4fcff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#a4fdac',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#d0dcdd' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#d0dcdd' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#7e8889',
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
