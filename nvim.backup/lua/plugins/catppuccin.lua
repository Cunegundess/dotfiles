return {
  "catppuccin/nvim",
  name = "catppuccin-mocha",
  priority = 1000,

  config = function()
    require("catppuccin").setup({
      transparent_background = true,
      term_colors = true,
      no_italic = false,
      no_bold = false,
      no_underline = false,
      styles = {
        comments = { "italic" },
        conditionals = { "italic" },
        loops = {},
        functions = {},
        keywords = {},
        strings = {},
        variables = {},
        numbers = {},
        booleans = {},
        properties = {},
        types = {},
        operators = {},
        integrations = {
          cmp = true,
          gitsigns = true,
          nvimtree = true,
          treesitter = true,
          notify = false,
          mini = {
            enabled = true,
            indentscope_color = "",
          },
        },
      },
    })
    --    vim.cmd.colorscheme("catppuccin-mocha")
  end,
}
