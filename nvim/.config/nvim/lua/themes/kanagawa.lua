return {
  {
    'rebelot/kanagawa.nvim',
    config = function()
      require('kanagawa').setup {
        compile = false,
        undercurl = true,
        commentStyle = { italic = true },
        functionStyle = { bold = true },
        keywordStyle = { italic = true },
        statementStyle = { bold = true },
        typeStyle = {},
        transparent = true,
        dimInactive = false,
        terminalColors = true,
        colors = {
          palette = {},
          theme = { wave = {}, lotus = {}, dragon = {}, all = { ui = { bg_gutter = 'none' } } },
        },
        overrides = function(colors)
          local theme = colors.theme
          return {
            TelescopeTitle = { fg = theme.ui.special, bold = true },
            TelescopePromptNormal = { bg = theme.ui.bg_p1 },
            TelescopePromptBorder = { fg = theme.ui.bg_p1, bg = theme.ui.bg_p1 },
            TelescopeResultsNormal = { fg = theme.ui.fg_dim, bg = theme.ui.bg_m1 },
            TelescopeResultsBorder = { fg = theme.ui.bg_m1, bg = theme.ui.bg_m1 },
            TelescopePreviewNormal = { bg = theme.ui.bg_dim },
            TelescopePreviewBorder = { bg = theme.ui.bg_dim, fg = theme.ui.bg_dim },

            Pmenu = { fg = theme.ui.shade0, bg = theme.ui.bg_p1 },
            PmenuSel = { fg = 'NONE', bg = theme.ui.bg_p2 },
            PmenuSbar = { bg = theme.ui.bg_m1 },
            PmenuThumb = { bg = theme.ui.bg_p2 },
          }
        end,
        theme = 'dragon',
        background = {
          dark = 'dragon',
          light = 'lotus',
        },
      }

      -- vim.cmd.colorscheme 'kanagawa'
    end,
  },
  {
    'thesimonho/kanagawa-paper.nvim',
    lazy = false,
    priority = 1000,
    config = function()
      require('kanagawa-paper').setup {
        -- enable undercurls for underlined text
        undercurl = true,
        -- transparent background
        transparent = false,
        -- highlight background for the left gutter
        gutter = false,
        -- background for diagnostic virtual text
        diag_background = true,
        -- dim inactive windows. Disabled when transparent
        dim_inactive = false,
        -- set colors for terminal buffers
        terminal_colors = true,
        -- cache highlights and colors for faster startup.
        -- see Cache section for more details.
        cache = false,

        styles = {
          -- style for comments
          comment = { italic = true },
          -- style for functions
          functions = { italic = false },
          -- style for keywords
          keyword = { italic = false, bold = false },
          -- style for statements
          statement = { italic = false, bold = false },
          -- style for types
          type = { italic = false },
        },
        -- override default palette and theme colors
        colors = {
          palette = {},
          theme = {
            ink = {},
            canvas = {},
          },
        },
        -- adjust overall color balance for each theme [-1, 1]
        color_offset = {
          ink = { brightness = 0, saturation = 0 },
          canvas = { brightness = 0, saturation = 0 },
        },
        -- override highlight groups
        overrides = function(colors)
          return {}
        end,

        -- uses lazy.nvim, if installed, to automatically enable needed plugins
        auto_plugins = true,
        -- enable highlights for all plugins (disabled if using lazy.nvim)
        all_plugins = package.loaded.lazy == nil,
        -- manually enable/disable individual plugins.
        -- check the `groups/plugins` directory for the exact names
        plugins = {
          -- examples:
          -- rainbow_delimiters = true
          -- which_key = false
        },

        -- enable integrations with other applications
        integrations = {
          -- automatically set wezterm theme to match the current neovim theme
          wezterm = {
            enabled = false,
            -- neovim will write the theme name to this file
            -- wezterm will read from this file to know which theme to use
            path = (os.getenv 'TEMP' or '/tmp') .. '/nvim-theme',
          },
        },
      }
    end,
  },
}
