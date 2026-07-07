local M = {}

M.theme_file = vim.fn.stdpath('config') .. '/.current_theme'

function M.save(name)
  local f = io.open(M.theme_file, 'w')
  if f then f:write(name) f:close() end
end

function M.load()
  local f = io.open(M.theme_file, 'r')
  if f then
    local name = f:read('*a'):gsub('\n', '')
    f:close()
    if name and name ~= '' then return name end
  end
  return nil
end

function M.apply(name)
  local theme = name or M.load() or 'vesper'
  pcall(vim.cmd.colorscheme, theme)
  M.save(theme)
end

local ok_vesper, vesper = pcall(require, 'vesper')
if ok_vesper then
  vesper.setup({
    transparent = false,
    italics = { comments = true, keywords = true, functions = false, strings = true, variables = false },
  })
end

vim.cmd.colorscheme('vesper')

return M
