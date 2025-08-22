return {
  { "ellisonleao/gruvbox.nvim", opts = { transparent_mode = true } },
  {
    "sainnhe/gruvbox-material",
    priority = 1000,
    config = function()
      vim.g.gruvbox_material_transparent_background = 1
      vim.g.gruvbox_material_foreground = "mix"
      vim.g.gruvbox_material_background = "hard"
      vim.g.gruvbox_material_ui_contrast = "high"
      vim.g.gruvbox_material_float_style = "bright"
      vim.g.gruvbox_material_statusline_style = "mix"
      vim.g.gruvbox_material_material_cursor = "auto"
    end,
  },
}
