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
      winopts = {
        -- split = 'belowright new', -- open in a split instead?
        -- "belowright new"  : split below
        -- "aboveleft new"   : split above
        -- "belowright vnew" : split right
        -- "aboveleft vnew   : split left
        -- Only valid when using a float window
        -- (i.e. when 'split' is not defined, default)
        height = 0.85, -- window height
        width = 0.85,  -- window width
        row = 0.35,    -- window row position (0=top, 1=bottom)
        col = 0.50,    -- window col position (0=left, 1=right)
        -- border argument passthrough to nvim_open_win()
        border = 'rounded',
        -- Backdrop opacity, 0 is fully opaque, 100 is fully transparent (i.e. disabled)
        backdrop = 60,
        -- title         = "Title",
        -- title_pos     = "center",        -- 'left', 'center' or 'right'
        -- title_flags   = false,           -- uncomment to disable title flags
        fullscreen = false, -- start fullscreen?
        -- enable treesitter highlighting for the main fzf window will only have
        -- effect where grep like results are present, i.e. "file:line:col:text"
        -- due to highlight color collisions will also override `fzf_colors`
        -- set `fzf_colors=false` or `fzf_colors.hl=...` to override
        treesitter = {
          enabled = true,
          fzf_colors = { ['hl'] = '-1:reverse', ['hl+'] = '-1:reverse' },
        },
        preview = {
          -- default     = 'bat',           -- override the default previewer?
          -- default uses the 'builtin' previewer
          border = 'rounded', -- preview border: accepts both `nvim_open_win`
          -- and fzf values (e.g. "border-top", "none")
          -- native fzf previewers (bat/cat/git/etc)
          -- can also be set to `fun(winopts, metadata)`
          wrap = false,             -- preview line wrap (fzf's 'wrap|nowrap')
          hidden = false,           -- start preview hidden
          vertical = 'up:30%',      -- up|down:size
          horizontal = 'right:40%', -- right|left:size
          layout = 'flex',          -- horizontal|vertical|flex
          flip_columns = 100,       -- #cols to switch to horizontal on flex
          -- Only used with the builtin previewer:
          title = true,             -- preview border title (file/buf)?
          title_pos = 'center',     -- left|center|right, title alignment
          scrollbar = 'float',      -- `false` or string:'float|border'
          -- float:  in-window floating border
          -- border: in-border "block" marker
          scrolloff = -1, -- float scrollbar offset from right
          -- applies only when scrollbar = 'float'
          delay = 10,     -- delay(ms) displaying the preview
          -- prevents lag on fast scrolling
          winopts = {     -- builtin previewer window options
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
      },
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
      -- vim.keymap.set('n', '<leader>sr', fzf_lua.resume, { desc = '[S]earch [R]esume' }),
      -- vim.keymap.set('n', '<leader>s.', fzf_lua.oldfiles, { desc = '[S]earch Recent Files ("." for repeat)' }),
      vim.keymap.set('n', '<leader><leader>', fzf_lua.buffers, { desc = '[ ] Find existing buffers' }),

      -- [[ LSP ]]

      vim.keymap.set('n', '<leader>gd', fzf_lua.lsp_definitions, { desc = '[G]oto [D]efinition' }),
      vim.keymap.set('n', '<leader>gr', fzf_lua.lsp_references, { desc = '[G]oto [R]eferences' }),
      vim.keymap.set('n', '<leader>gI', fzf_lua.lsp_implementations, { desc = '[G]oto [I]mplementation' }),
      vim.keymap.set('n', '<leader>D', fzf_lua.lsp_typedefs, { desc = 'Type [D]efinition' }),
      vim.keymap.set('n', '<leader>sd', fzf_lua.lsp_document_diagnostics, { desc = '[S]earch [D]iagnostics' }),
      vim.keymap.set('n', '<leader>ss', fzf_lua.lsp_document_symbols, { desc = '[S]earch [S]ymbols' }),
      -- vim.keymap.set('n', '<leader>ws', fzf_lua.lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')
      -- vim.keymap.set('n', '<leader>rn', fzf_lua.lsp_rename, '[R]e[n]ame')
      vim.keymap.set('n', '<leader>ca', fzf_lua.lsp_code_actions, { desc = '[C]ode [A]ction' }),
      vim.keymap.set('n', '<leader>gD', fzf_lua.lsp_declarations, { desc = '[G]oto [D]eclaration' }),
      vim.keymap.set('n', '<leader>gs', fzf_lua.git_status, { desc = '[G]it [S]tatus' }),
      vim.keymap.set('n', '<leader>gc', fzf_lua.git_commits, { desc = '[G]it [C]ommits' }),
      vim.keymap.set('n', '<leader>gb', fzf_lua.git_branches, { desc = '[G]it [B]ranches' }),
      vim.keymap.set('n', '<leader>gB', fzf_lua.git_blame, { desc = '[G]it [B]lame' }),
    }
  end,
}
