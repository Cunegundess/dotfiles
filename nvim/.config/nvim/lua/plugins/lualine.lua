-- if true then
--   return {}
-- end

return {
  'nvim-lualine/lualine.nvim',
  dependencies = {
    'nvim-tree/nvim-web-devicons',
    'meuter/lualine-so-fancy.nvim',
  },

  config = function()
    local function clock()
      return os.date '%d/%m 󰥔 %H:%M'
    end

    require('lualine').setup {
      options = {
        icons_enabled = true,
        theme = 'auto',
        component_separators = '',
        section_separators = '',
        globalstatus = true,
        disabled_filetypes = { 'neo-tree', 'oil' },
      },

      sections = {
        lualine_a = {},

        lualine_b = {
          { 'mode' },
          { 'fancy_branch', icon = '' },
          { 'fancy_diff' },
        },

        lualine_c = {
          -- {
          --   'fancy_cwd',
          --   substitute_home = true,
          -- },
          {
            'filename',
            path = 1,
            symbols = { modified = '●', readonly = '', unnamed = '' },
          },
        },

        lualine_x = {
          { 'fancy_diagnostics' },
          { 'fancy_lsp_servers' },
        },

        lualine_y = {
          clock,
          { 'filetype', icon_only = true },
        },
        lualine_z = {},
      },

      inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = {
          {
            'filename',
            path = 1,
            symbols = { modified = '●', readonly = '', unnamed = '' },
          },
        },
        lualine_x = { 'location' },
        lualine_y = {},
        lualine_z = {},
      },

      extensions = { 'nvim-dap-ui', 'oil', 'quickfix' },
    }
  end,
}
