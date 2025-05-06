return {
  "Cunegundess/notes-nvim",
  lazy = false,
  config = function()
    require("notes-nvim").setup {
      target_file = "~/Documentos/notes/notes.md",
      border = "rounded",
    }
  end,
}
