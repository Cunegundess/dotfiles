return {
  "Cunegundess/notes-nvim",
  cmd = { "Notes" },
  config = function()
    require("notes-nvim").setup {
      target_file = "~/Documentos/notes/notes.md",
      border = "rounded",
    }
  end,
}
