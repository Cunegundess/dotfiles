return {
  "tpope/vim-fugitive",
  config = function()
    local myFugitive = vim.api.nvim_create_augroup("myFugitive", {})

    local autocmd = vim.api.nvim_create_autocmd
    autocmd("BufWinEnter", {
      group = myFugitive,
      pattern = "*",
      callback = function()
        if vim.bo.ft ~= "fugitive" then
          return
        end
      end,
    })
  end,
}
