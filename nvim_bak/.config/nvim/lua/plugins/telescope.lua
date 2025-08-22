return {
  "nvim-telescope/telescope.nvim",
  event = "VimEnter",
  branch = "0.1.x",
  dependencies = {
    "nvim-lua/plenary.nvim",
    {
      "nvim-telescope/telescope-fzf-native.nvim",
      build = "make",
      cond = function()
        return vim.fn.executable("make") == 1
      end,
    },
    { "nvim-telescope/telescope-ui-select.nvim" },
    { "nvim-tree/nvim-web-devicons", enabled = vim.g.have_nerd_font },
  },
  keys = {
    {
      "<leader>fp",
      function()
        require("telescope.builtin").find_files({ cwd = require("lazy.core.config").options.root })
      end,
      desc = "Find Plugin File",
    },
  },
  opts = function(_, opts)
    local themes = require("telescope.themes")

    -- 🔥 zera os defaults herdados do LazyVim (que forçam dropdown)
    opts.defaults = {}

    -- agora aplica o ivy como tema global
    opts.defaults = themes.get_ivy({
      file_ignore_patterns = { "node_modules", "git", "venv" },
      previewer = true,
      layout_config = { height = 0.4 },
    })

    opts.pickers = vim.tbl_deep_extend("force", opts.pickers or {}, {
      colorscheme = { enable_preview = true },
      find_files = { hidden = true },
    })

    opts.extensions = vim.tbl_deep_extend("force", opts.extensions or {}, {
      ["ui-select"] = themes.get_ivy(),
    })
  end,
}
