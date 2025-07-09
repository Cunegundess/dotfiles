return {
  'ibhagwan/fzf-lua',
  -- dependencies = { 'nvim-tree/nvim-web-devicons' },
  dependencies = { 'echasnovski/mini.icons' },
  config = function()
    local fzf_lua = require 'fzf-lua'

    fzf_lua.setup {
      winopts = {
        -- split = 'belowright new', -- open in a split instead?
        -- "belowright new"  : split below
        -- "aboveleft new"   : split above
        -- "belowright vnew" : split right
        -- "aboveleft vnew   : split left
        -- Only valid when using a float window
        -- (i.e. when 'split' is not defined, default)
        height = 0.90,
        width = 0.90,
        row = 0.35, -- window row position (0=top, 1=bottom)
        col = 0.50, -- window col position (0=left, 1=right)
        -- border argument passthrough to nvim_open_win()
        border = 'rounded',
        -- Backdrop opacity, 0 is fully opaque, 100 is fully transparent (i.e. disabled)
        backdrop = 60,
        fullscreen = false, -- start fullscreen?
        treesitter = {
          enabled = true,
          fzf_colors = { ['hl'] = '-1:reverse', ['hl+'] = '-1:reverse' },
        },
        preview = {
          -- default = 'bat',
          border = 'rounded',
          wrap = false,
          hidden = false,
          vertical = 'up:40%',
          horizontal = 'right:50%',
          layout = 'flex',
          flip_columns = 100,
          scrolloff = -1,
          delay = 0,
          winopts = {
            number = true,
            relativenumber = true,
            cursorline = true,
            cursorlineopt = 'both',
            cursorcolumn = false,
            signcolumn = 'no',
            list = false,
            foldenable = false,
            foldmethod = 'manual',
          },
        },
        fzf_colors = true,
      },

      vim.keymap.set('n', '<leader>st', function()
        fzf_lua.colorschemes {
          preview = function(entry)
            vim.cmd.colorscheme(entry)
          end,
          actions = {
            ['default'] = function(selected)
              local theme = selected[1]:gsub('%.vim$', '')
              vim.cmd.colorscheme(theme)
              _G.save_colorscheme(theme)
            end,
          },
        }
      end, { desc = '[S]earch [T]heme' }),
      vim.keymap.set('n', '<leader>sh', fzf_lua.help_tags, { desc = '[S]earch [H]elp' }),
      vim.keymap.set('n', '<leader>sk', fzf_lua.keymaps, { desc = '[S]earch [K]eymaps' }),
      vim.keymap.set('n', '<leader>sf', fzf_lua.files, { desc = '[S]earch [F]iles' }),
      vim.keymap.set('n', '<leader>sw', fzf_lua.live_grep, { desc = '[S]earch current [W]ord' }),
      vim.keymap.set('n', '<leader>sg', fzf_lua.grep_visual, { desc = '[S]earch by [G]rep' }),
      vim.keymap.set('n', '<leader><leader>', fzf_lua.buffers, { desc = '[ ] Find existing buffers' }),

      -- [[ LSP ]]
      vim.keymap.set('n', '<leader>gd', fzf_lua.lsp_definitions, { desc = '[G]oto [D]efinition' }),
      vim.keymap.set('n', '<leader>gr', fzf_lua.lsp_references, { desc = '[G]oto [R]eferences' }),
      vim.keymap.set('n', '<leader>gI', fzf_lua.lsp_implementations, { desc = '[G]oto [I]mplementation' }),
      vim.keymap.set('n', '<leader>D', fzf_lua.lsp_typedefs, { desc = 'Type [D]efinition' }),
      vim.keymap.set('n', '<leader>sd', fzf_lua.lsp_document_diagnostics, { desc = '[S]earch [D]iagnostics' }),
      vim.keymap.set('n', '<leader>ss', fzf_lua.lsp_document_symbols, { desc = '[S]earch [S]ymbols' }),
      vim.keymap.set('n', '<leader>ca', fzf_lua.lsp_code_actions, { desc = '[C]ode [A]ction' }),
      vim.keymap.set('n', '<leader>gD', fzf_lua.lsp_declarations, { desc = '[G]oto [D]eclaration' }),

      -- [[ Git ]]
      vim.keymap.set('n', '<leader>gs', fzf_lua.git_status, { desc = '[G]it [S]tatus' }),
      vim.keymap.set('n', '<leader>gc', fzf_lua.git_commits, { desc = '[G]it [C]ommits' }),
      vim.keymap.set('n', '<leader>gb', fzf_lua.git_branches, { desc = '[G]it [B]ranches' }),
      vim.keymap.set('n', '<leader>gB', fzf_lua.git_blame, { desc = '[G]it [B]lame' }),
    }
  end,
}
