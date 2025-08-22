return {
  "datsfilipe/vesper.nvim",
  config = function()
    require("vesper").setup({
      transparent = false,
      italics = {
        comments = true,
        keywords = true,
        functions = true,
        strings = true,
        variables = true,
      },
    })
  end,
}
