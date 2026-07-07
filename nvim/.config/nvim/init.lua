_G.Config = {}

local gr = vim.api.nvim_create_augroup('custom-config', {})
Config.new_autocmd = function(event, pattern, callback, desc)
  local opts = { group = gr, pattern = pattern, callback = callback, desc = desc }
  vim.api.nvim_create_autocmd(event, opts)
end

Config.on_packchanged = function(plugin_name, kinds, callback, desc)
  local f = function(ev)
    local name, kind = ev.data.spec.name, ev.data.kind
    if not (name == plugin_name and vim.tbl_contains(kinds, kind)) then return end
    if not ev.data.active then vim.cmd.packadd(plugin_name) end
    callback(ev.data)
  end
  Config.new_autocmd('PackChanged', '*', f, desc)
end

local safe = function(fn, ...)
  local ok, result = pcall(fn, ...)
  if not ok then
    vim.notify('[init] ' .. tostring(result), vim.log.levels.WARN)
  end
  return result
end

Config.now = function(f) safe(f) end
Config.later = function(f) vim.defer_fn(function() safe(f) end, 0) end

vim.pack.add({
  -- Core
  'https://github.com/echasnovski/mini.nvim',

  -- Theme
  'https://github.com/datsfilipe/vesper.nvim',

  -- LSP & Completion
  'https://github.com/saghen/blink.cmp',
  'https://github.com/neovim/nvim-lspconfig',
  'https://github.com/williamboman/mason.nvim',
  'https://github.com/WhoIsSethDaniel/mason-tool-installer.nvim',

  -- Treesitter
  'https://github.com/nvim-treesitter/nvim-treesitter',
  'https://github.com/nvim-treesitter/nvim-treesitter-textobjects',

  -- DAP
  'https://github.com/mfussenegger/nvim-dap',
  'https://github.com/rcarriga/nvim-dap-ui',
  'https://github.com/nvim-neotest/nvim-nio',
  'https://github.com/mfussenegger/nvim-dap-python',

  -- Dadbod
  'https://github.com/tpope/vim-dadbod',
  'https://github.com/kristijanhusak/vim-dadbod-ui',
  'https://github.com/kristijanhusak/vim-dadbod-completion',

  -- Picker
  'https://github.com/ibhagwan/fzf-lua',
  'https://github.com/nvim-lua/plenary.nvim',

  -- Formatting
  'https://github.com/stevearc/conform.nvim',

  -- Git
  'https://github.com/lewis6991/gitsigns.nvim',
  'https://github.com/NeogitOrg/neogit',

  -- UI
  'https://github.com/folke/which-key.nvim',
  'https://github.com/stevearc/oil.nvim',
  'https://github.com/folke/trouble.nvim',
  'https://github.com/MeanderingProgrammer/render-markdown.nvim',
})
