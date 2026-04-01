local M = {}

M.theme_file = vim.fn.stdpath 'config' .. '/.current_theme'

function M.save(name)
  local f = io.open(M.theme_file, 'w')
  if f then
    f:write(name)
    f:close()
  end
end

function M.load()
  local f = io.open(M.theme_file, 'r')
  if f then
    local name = f:read '*a'
    f:close()
    name = name:gsub('\n', '')
    if name and name ~= '' then
      return name
    end
  end
  return nil
end

function M.apply(name)
  local theme = name or M.load() or 'catppuccin-mocha'
  vim.cmd.colorscheme(theme)
  M.save(theme)
end

function M.refresh()
  vim.cmd.TSUpdateSync()
  vim.cmd.redrawstatus()
end

vim.api.nvim_create_user_command('ThemeRefresh', function()
  require('plugins.theme').refresh()
end, {})

return M
