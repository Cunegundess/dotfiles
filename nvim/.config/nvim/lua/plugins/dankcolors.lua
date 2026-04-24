return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#101414',
				base01 = '#101414',
				base02 = '#838d8e',
				base03 = '#838d8e',
				base04 = '#d8e4e5',
				base05 = '#f8feff',
				base06 = '#f8feff',
				base07 = '#f8feff',
				base08 = '#ff9fc0',
				base09 = '#ff9fc0',
				base0A = '#a7e8f0',
				base0B = '#a5ffae',
				base0C = '#d6faff',
				base0D = '#a7e8f0',
				base0E = '#bff7ff',
				base0F = '#bff7ff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#838d8e',
				fg = '#f8feff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#a7e8f0',
				fg = '#101414',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#838d8e' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#d6faff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#bff7ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#a7e8f0',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#a7e8f0',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#d6faff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#a5ffae',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#d8e4e5' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#d8e4e5' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#838d8e',
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
