return {
  "utilyre/barbecue.nvim",
  lazy = false,
  name = "barbecue",
  version = "*",
  dependencies = {
    "SmiteshP/nvim-navic",
    "nvim-tree/nvim-web-devicons", -- optional dependency
    },
  config = function()
    require("barbecue.ui").toggle(true)
  end,
}
