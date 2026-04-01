local M = {}

M.name = "catppuccin-mocha"

function M.apply()
  vim.cmd.colorscheme(M.name)
end

return M
