return {
  "mistweaverco/kulala.nvim",
  ft = { "http", "rest" },
  opts = {
    global_keymaps_prefix = "<leader>R",
    global_keymaps = {
      ["Send request"] = { -- sets global mapping
        "<leader>Rs",
        function()
          require("kulala").run()
        end,
        mode = { "n", "v" }, -- optional mode, default is n
        desc = "Send request", -- optional description, otherwise inferred from the key
      },
      ["Send all requests"] = {
        "<leader>Ra",
        function()
          require("kulala").run_all()
        end,
        mode = { "n", "v" },
        ft = "http", -- sets mapping for *.http files only
      },
      ["Replay the last request"] = {
        "<leader>Rr",
        function()
          require("kulala").replay()
        end,
        ft = { "http", "rest" }, -- sets mapping for specified file types
      },
      ["Find request"] = false, -- set to false to disable
    },
    ui = {
      -- possible values: "split", "float"
      display_mode = "split",
      -- possible values: "vertical", "horizontal"
      split_direction = "horizontal",
      default_view = "body", ---@type "body"|"headers"|"headers_body"|"verbose"|fun(response: Response)
      -- Current available pane contains { "body", "headers", "headers_body", "script_output", "stats", "verbose", "report", "help" },
      default_winbar_panes = { "body", "headers", "headers_body", "verbose", "script_output", "report", "help" },
      -- icons position: "signcolumn"|"on_request"|"above_request"|"below_request" or nil to disable
      show_icons = "signcolumn",
      icons = {
        inlay = {
          loading = "⏳",
          done = "✅",
          error = "❌",
        },
        lualine = "🐼",
        textHighlight = "WarningMsg", -- highlight group for request elapsed time
      },
    },
  },
}
