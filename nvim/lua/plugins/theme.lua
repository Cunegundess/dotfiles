return {
  {
    "tiagovla/tokyodark.nvim",
    config = function()
      require("tokyodark").setup({
        transparent_background = true, -- set background to transparent
        gamma = 1.00,                 -- adjust the brightness of the theme
        styles = {
          comments = { italic = true }, -- style for comments
          keywords = { italic = true }, -- style for keywords
          identifiers = { italic = true }, -- style for identifiers
          functions = {},             -- style for functions
          variables = {},             -- style for variables
        },
        terminal_colors = true, -- enable terminal colors
      })                    -- calling setup is optional
      -- vim.cmd([[colorscheme tokyodark]])
    end,
  },
  {
    "ribru17/bamboo.nvim",
    config = function()
      require("bamboo").setup({
        -- NOTE: to use the light theme, set `vim.o.background = 'light'`
        style = "vulgaris",                                   -- Choose between 'vulgaris' (regular), 'multiplex' (greener), and 'light'
        toggle_style_key = nil,                               -- Keybind to toggle theme style. Leave it nil to disable it, or set it to a string, e.g. "<leader>ts"
        toggle_style_list = { "vulgaris", "multiplex", "light" }, -- List of styles to toggle between
        transparent = true,                                  -- Show/hide background
        dim_inactive = false,                                 -- Dim inactive windows/buffers
        term_colors = true,                                   -- Change terminal color as per the selected theme style
        ending_tildes = false,                                -- Show the end-of-buffer tildes. By default they are hidden
        cmp_itemkind_reverse = false,                         -- reverse item kind highlights in cmp menu

        -- Change code style ---
        -- Options are anything that can be passed to the `vim.api.nvim_set_hl` table
        -- You can also configure styles with a string, e.g. keywords = 'italic,bold'
        code_style = {
          comments = { italic = true },
          conditionals = { italic = true },
          keywords = {},
          functions = {},
          namespaces = { italic = true },
          parameters = { italic = true },
          strings = {},
          variables = {},
        },

        -- Lualine options --
        lualine = {
          transparent = true, -- lualine center bar transparency
        },

        -- Custom Highlights --
        colors = {}, -- Override default colors
        highlights = {}, -- Override highlight groups

        -- Plugins Config --
        diagnostics = {
          darker = false, -- darker colors for diagnostic
          undercurl = true, -- use undercurl instead of underline for diagnostics
          background = true, -- use background color for virtual text
        },
      })
      -- require("bamboo").load()
    end,
  },
  {
    "deparr/tairiki.nvim",
    config = function()
      require("tairiki").setup({
        -- Main options --
        style = "dark",           -- Default theme style. Choose between 'dark' (more styles on the way)
        transparent = true,      -- Show/hide background
        term_colors = true,       -- Change terminal color as per the selected theme style
        ending_tildes = false,    -- Show the end-of-buffer tildes. By default they are hidden
        cmp_itemkind_reverse = false, -- reverse item kind highlights in cmp menu
        visual_bold = false,      -- bolden visual selections

        -- toggle theme style ---
        toggle_style_key = nil,     -- keybind to toggle theme style. Leave it nil to disable it, or set it to a string, for example "<leader>ts"
        toggle_style_list = { "dark" }, -- List of styles to toggle between TODO

        -- Change code style ---
        -- Options are italic, bold, underline, none
        -- You can configure multiple style with comma separated, For e.g., keywords = 'italic,bold'
        code_style = {
          comments = "italic",
          keywords = "none",
          functions = "none",
          strings = "none",
          variables = "none",
        },

        -- Lualine options --
        lualine = {
          transparent = true, -- lualine center bar transparency
        },

        -- Custom Highlights --
        colors = {}, -- Override default colors
        highlights = {}, -- Override highlight groups

        -- Plugins Config --
        diagnostics = {
          darker = true, -- darker colors for diagnostic
          undercurl = true, -- use undercurl instead of underline for diagnostics
          background = true, -- use background color for virtual text
        },
      })
      -- require("tairiki").load() -- only necessary to use as default theme, has same behavior as ':colorscheme tairiki'
    end,
  },
  {
    "DanielEliasib/sweet-fusion",
    name = "sweet-fusion",
    opts = {
      terminal_colors = true,
      transparency = true,
      hl_styles = {
        comments = { italic = true },
        keywords = { italic = true },
        functions = {},
        variables = {},
      },
      dim_inactive = true,
    },
  },
  {
    "gmr458/vscode_modern_theme.nvim",
    config = function()
      require("vscode_modern").setup({
        cursorline = true,
        transparent_background = true,
        nvim_tree_darker = true,
      })
      -- vim.cmd.colorscheme("vscode_modern")
    end,
  },
  {
    "loctvl842/monokai-pro.nvim",
    config = function()
      require("monokai-pro").setup({
        transparent_background = true,
        terminal_colors = true,
        devicons = true, -- highlight the icons of `nvim-web-devicons`
        styles = {
          comment = { italic = true },
          keyword = { italic = true },  -- any other keyword
          type = { italic = true },     -- (preferred) int, long, char, etc
          storageclass = { italic = true }, -- static, register, volatile, etc
          structure = { italic = true }, -- struct, union, enum, etc
          parameter = { italic = true }, -- parameter pass in function
          annotation = { italic = true },
          tag_attribute = { italic = true }, -- attribute of tag in reactjs
        },
        filter = "pro",                 -- classic | octagon | pro | machine | ristretto | spectrum
        -- Enable this will disable filter option
        day_night = {
          enable = false,       -- turn off by default
          day_filter = "pro",   -- classic | octagon | pro | machine | ristretto | spectrum
          night_filter = "spectrum", -- classic | octagon | pro | machine | ristretto | spectrum
        },
        inc_search = "background", -- underline | background
        background_clear = {
          -- "float_win",
          "toggleterm",
          "telescope",
          -- "which-key",
          "renamer",
          "notify",
          -- "nvim-tree",
          -- "neo-tree",
          -- "bufferline", -- better used if background of `neo-tree` or `nvim-tree` is cleared
        }, -- "float_win", "toggleterm", "telescope", "which-key", "renamer", "neo-tree", "nvim-tree", "bufferline"
        plugins = {
          bufferline = {
            underline_selected = false,
            underline_visible = false,
          },
          indent_blankline = {
            context_highlight = "default", -- default | pro
            context_start_underline = false,
          },
        },
      })
    end,
  },
  {
    "sainnhe/gruvbox-material",
  },
  {
    "ricardoraposo/gruvbox-minor.nvim",
  },
}
