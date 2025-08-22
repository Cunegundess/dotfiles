-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
--
-- Add any additional autocmds here
-- with `vim.api.nvim_create_autocmd`
--
-- Or remove existing autocmds by their group name (which is prefixed with `lazyvim_` for the defaults)
-- e.g. vim.api.nvim_del_augroup_by_name("lazyvim_wrap_spell")

-- Autoformat setting
local set_autoformat = function(pattern, bool_val)
  vim.api.nvim_create_autocmd({ "FileType" }, {
    desc = "Disable autoformat per language",
    pattern = pattern,
    callback = function()
      vim.b.autoformat = bool_val
    end,
  })
end

set_autoformat({ "python" }, false)
set_autoformat({ "c" }, false)
set_autoformat({ "lua" }, true)
set_autoformat({ "markdown" }, true)

vim.api.nvim_create_autocmd("FileType", {
  desc = "Disable spelling in markdown files",
  pattern = { "markdown" },
  command = "setlocal nospell",
})

vim.api.nvim_create_autocmd("BufEnter", {
  desc = "Quit Neovim if more than one window is open and only sidebar windows are list",
  callback = function()
    local wins = vim.api.nvim_tabpage_list_wins(0)

    -- Both neo-tree and aerial will auto-quit if there is only a single window left
    if #wins <= 1 then
      return
    end

    local sidebar_fts = { aerial = true, ["neo-tree"] = true }

    for _, winid in ipairs(wins) do
      if vim.api.nvim_win_is_valid(winid) then
        local bufnr = vim.api.nvim_win_get_buf(winid)
        local filetype = vim.bo[bufnr].filetype

        -- If any visible windows are not sidebars, early return
        if not sidebar_fts[filetype] then
          return

        -- If the visible window is a sidebar
        else
          -- Only count filetypes once, so remove a found sidebar from the detection
          sidebar_fts[filetype] = nil
        end
      end
    end

    if #vim.api.nvim_list_tabpages() > 1 then
      vim.cmd.tabclose()
    else
      vim.cmd.qall()
    end
  end,
})
