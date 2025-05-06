return {
  "NeogitOrg/neogit",
  lazy = false,
  dependencies = {
    "nvim-lua/plenary.nvim",
    "sindrets/diffview.nvim",

    -- Only one of these is needed.
    "nvim-telescope/telescope.nvim", -- optional
    -- "ibhagwan/fzf-lua", -- optional
    -- "echasnovski/mini.pick", -- optional
    -- "folke/snacks.nvim", -- optional
  },
  init = function()
    local map = vim.keymap.set
    map("n", "<leader>gg", "<cmd>Neogit<CR>", { desc = "Neogit Open" })
  end,
}
