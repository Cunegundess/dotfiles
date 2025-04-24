return {
  'ibhagwan/fzf-lua',
  -- optional for icon support
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  -- or if using mini.icons/mini.nvim
  -- dependencies = { "echasnovski/mini.icons" },
  config = function()
    local fzf_lua = require 'fzf-lua'

    fzf_lua.setup {
      -- MISC GLOBAL SETUP OPTIONS, SEE BELOW
      -- fzf_bin = ...,
      -- each of these options can also be passed as function that return options table
      -- e.g. winopts = function() return { ... } end
      -- winopts = { ... },    -- UI Options
      -- keymap = { ... },     -- Neovim keymaps / fzf binds
      -- actions = { ... },    -- Fzf "accept" binds
      -- fzf_opts = { ... },   -- Fzf CLI flags
      -- fzf_colors = { ... }, -- Fzf `--color` specification
      -- hls = { ... },        -- Highlights
      -- previewers = { ... }, -- Previewers options
      -- SPECIFIC COMMAND/PICKER OPTIONS, SEE BELOW
      -- files = { ... },

      vim.keymap.set('n', '<leader>st', fzf_lua.colorschemes, { desc = '[S]earch [T]heme' }),
      vim.keymap.set('n', '<leader>sh', fzf_lua.help_tags, { desc = '[S]earch [H]elp' }),
      vim.keymap.set('n', '<leader>sk', fzf_lua.keymaps, { desc = '[S]earch [K]eymaps' }),
      vim.keymap.set('n', '<leader>sf', fzf_lua.files, { desc = '[S]earch [F]iles' }),
      -- vim.keymap.set('n', '<leader>ss', fzf_lua.fzf_lua, { desc = '[S]earch [S]elect Telescope' }),
      -- vim.keymap.set('n', '<leader>sw', fzf_lua.grep_string, { desc = '[S]earch current [W]ord' }),
      vim.keymap.set('n', '<leader>sg', fzf_lua.grep_visual, { desc = '[S]earch by [G]rep' }),
      vim.keymap.set('n', '<leader>sd', fzf_lua.lsp_document_diagnostics, { desc = '[S]earch [D]iagnostics' }),
      vim.keymap.set('n', '<leader>ss', fzf_lua.lsp_document_symbols, { desc = '[S]earch [D]iagnostics' }),
      -- vim.keymap.set('n', '<leader>sr', fzf_lua.resume, { desc = '[S]earch [R]esume' }),
      -- vim.keymap.set('n', '<leader>s.', fzf_lua.oldfiles, { desc = '[S]earch Recent Files ("." for repeat)' }),
      vim.keymap.set('n', '<leader><leader>', fzf_lua.buffers, { desc = '[ ] Find existing buffers' }),

      -- [[ LSP ]]

      vim.keymap.set('n', 'gd', fzf_lua.lsp_definitions, { desc = '[G]oto [D]efinition' }),
      vim.keymap.set('n', 'gr', fzf_lua.lsp_references, { desc = '[G]oto [R]eferences' }),
      vim.keymap.set('n', 'gI', fzf_lua.lsp_implementations, { desc = '[G]oto [I]mplementation' }),
      -- vim.keymap.set('n', '<leader>D', fzf_lua.lsp_type_definitions, 'Type [D]efinition')
      -- vim.keymap.set('n', '<leader>ds', fzf_lua.lsp_document_symbols, '[D]ocument [S]ymbols')
      -- vim.keymap.set('n', '<leader>ws', fzf_lua.lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')
      -- vim.keymap.set('n', '<leader>rn', fzf_lua.lsp_rename, '[R]e[n]ame')
      vim.keymap.set('n', '<leader>ca', fzf_lua.lsp_code_actions, { desc = '[C]ode [A]ction' }),
      vim.keymap.set('n', 'gD', fzf_lua.lsp_declarations, { desc = '[G]oto [D]eclaration' }),
      -- vim.keymap.set('n', '<leader>vf', vim.diagnostic.open_float, opts)
    }
  end,
}
