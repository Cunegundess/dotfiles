return {
  'stevearc/conform.nvim',
  event = { 'BufWritePre' },
  cmd = { 'ConformInfo' },
  keys = {
    {
      '<leader>cf',
      function()
        require('conform').format {
          async = true,
          lsp_fallback = true,
        }
      end,
      mode = '',
      desc = '[C]ode Format',
    },
  },
  opts = {
    notify_on_error = true,
    format_on_save = function(bufnr)
      local disable_filetypes = { python = true }
      return {
        timeout_ms = 500,
        lsp_fallback = not disable_filetypes[vim.bo[bufnr].filetype],
      }
    end,

    formatters_by_ft = {
      lua = { 'stylua' },
      nix = { 'nixfmt' },
      -- python = { 'ruff' },
    },
  },
}
