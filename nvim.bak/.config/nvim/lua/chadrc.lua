-- This file needs to have same structure as nvconfig.lua
-- https://github.com/NvChad/ui/blob/v3.0/lua/nvconfig.lua
-- Please read that file to know all available options :(

---@type ChadrcConfig
local M = {}

M.base46 = {
  theme = "tokyonight",
  transparency = true,

  hl_override = {
    Comment = { italic = true },
    ["@comment"] = { italic = true },
  },
}

M.ui = {
  tabufline = {
    enabled = true,
    lazyload = false,
  },

  statusline = {
    theme = "default",
    separator_style = "block",
  },
}

M.nvdash = { load_on_startup = true }

M.lsp = { signature = true }

M.cheatsheet = { theme = "simple" }

M.plugins = {
  ["nvim-telescope/telescope.nvim"] = {
    override_options = function()
      return {
        defaults = {
          vimgrep_arguments = {
            "--hidden",
          },
        },
      }
    end,
  },
}

return M
