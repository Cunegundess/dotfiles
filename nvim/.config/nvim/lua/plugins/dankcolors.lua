return {
	{
		"RRethy/base16-nvim",
		priority = 1000,
		config = function()
			require('base16-colorscheme').setup({
				base00 = '#121314',
				base01 = '#121314',
				base02 = '#83898e',
				base03 = '#83898e',
				base04 = '#d8dfe5',
				base05 = '#f8fcff',
				base06 = '#f8fcff',
				base07 = '#f8fcff',
				base08 = '#ff9fbc',
				base09 = '#ff9fbc',
				base0A = '#cbe1f0',
				base0B = '#a5ffb0',
				base0C = '#eaf6ff',
				base0D = '#cbe1f0',
				base0E = '#def1ff',
				base0F = '#def1ff',
			})

			vim.api.nvim_set_hl(0, 'Visual', {
				bg = '#83898e',
				fg = '#f8fcff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Statusline', {
				bg = '#cbe1f0',
				fg = '#121314',
			})
			vim.api.nvim_set_hl(0, 'LineNr', { fg = '#83898e' })
			vim.api.nvim_set_hl(0, 'CursorLineNr', { fg = '#eaf6ff', bold = true })

			vim.api.nvim_set_hl(0, 'Statement', {
				fg = '#def1ff',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Keyword', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Repeat', { link = 'Statement' })
			vim.api.nvim_set_hl(0, 'Conditional', { link = 'Statement' })

			vim.api.nvim_set_hl(0, 'Function', {
				fg = '#cbe1f0',
				bold = true
			})
			vim.api.nvim_set_hl(0, 'Macro', {
				fg = '#cbe1f0',
				italic = true
			})
			vim.api.nvim_set_hl(0, '@function.macro', { link = 'Macro' })

			vim.api.nvim_set_hl(0, 'Type', {
				fg = '#eaf6ff',
				bold = true,
				italic = true
			})
			vim.api.nvim_set_hl(0, 'Structure', { link = 'Type' })

			vim.api.nvim_set_hl(0, 'String', {
				fg = '#a5ffb0',
				italic = true
			})

			vim.api.nvim_set_hl(0, 'Operator', { fg = '#d8dfe5' })
			vim.api.nvim_set_hl(0, 'Delimiter', { fg = '#d8dfe5' })
			vim.api.nvim_set_hl(0, '@punctuation.bracket', { link = 'Delimiter' })
			vim.api.nvim_set_hl(0, '@punctuation.delimiter', { link = 'Delimiter' })

			vim.api.nvim_set_hl(0, 'Comment', {
				fg = '#83898e',
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
