return {
  "goolord/alpha-nvim",
  dependencies = {
    "nvim-tree/nvim-web-devicons",
  },

  config = function()
    local alpha = require("alpha")
    local dashboard = require("alpha.themes.dashboard")

    dashboard.section.header.val = {
      [[                                                                       ]],
      [[                                                                       ]],
      [[                                                                       ]],
      [[                                                                       ]],
      [[                                                                     ]],
      [[       ████ ██████           █████      ██                     ]],
      [[      ███████████             █████                             ]],
      [[      █████████ ███████████████████ ███   ███████████   ]],
      [[     █████████  ███    █████████████ █████ ██████████████   ]],
      [[    █████████ ██████████ █████████ █████ █████ ████ █████   ]],
      [[  ███████████ ███    ███ █████████ █████ █████ ████ █████  ]],
      [[ ██████  █████████████████████ ████ █████ █████ ████ ██████ ]],
      [[                                                                       ]],
      [[                                                                       ]],
      [[                                                                       ]],
    }

    --buttons = {
    --{ "  Find File", "Spc f f", "Telescope find_files" },
    --{ "󰈚  Recent Files", "Spc f o", "Telescope oldfiles" },
    --{ "󰈭  Find Word", "Spc f w", "Telescope live_grep" },
    --{ "  Bookmarks", "Spc m a", "Telescope marks" },
    --  { "  Themes", "Spc t h", "Telescope themes" },
    --    { "  Mappings", "Spc c h", "NvCheatsheet" },
    --    },

    alpha.setup(dashboard.opts)
  end,
}
