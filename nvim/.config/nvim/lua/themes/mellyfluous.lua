return {
  'ramojus/mellifluous.nvim',
  lazy = false,
  priority = 1000,
  config = function()
    require('mellifluous').setup {
      styles = { -- see :h attr-list for options. set {} for NONE, { option = true } for option
        main_keywords = { bold = true },
      },
      mellifluous = {
        neutral = true, -- set this to false for original mellifluous (when it was called meliora theme)
      },
    }
  end,
}
