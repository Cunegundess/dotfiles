return {
  'hrsh7th/nvim-cmp',
  event = 'InsertEnter',
  dependencies = {
    {
      'L3MON4D3/LuaSnip',
      build = (function()
        if vim.fn.has 'win32' == 1 or vim.fn.executable 'make' == 0 then
          return
        end
        return 'make install_jsregexp'
      end)(),
      dependencies = {
        {
          'rafamadriz/friendly-snippets',
          config = function()
            require('luasnip.loaders.from_vscode').lazy_load()
          end,
        },
      },
    },
    'saadparwaiz1/cmp_luasnip',
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-path',
    'onsails/lspkind.nvim',
  },
  config = function()
    local cmp = require 'cmp'
    local luasnip = require 'luasnip'
    local lspkind = require 'lspkind'

    local kind_icons = {
      Text = '',
      Method = '󰆧',
      Function = '󰊕',
      Constructor = '',
      Field = '󰇽',
      Variable = '󰂡',
      Class = '󰠱',
      Interface = '',
      Module = '',
      Property = '󰜢',
      Unit = '',
      Value = '󰎠',
      Enum = '',
      Keyword = '󰌋',
      Snippet = '',
      Color = '󰏘',
      File = '󰈙',
      Reference = '',
      Folder = '󰉋',
      EnumMember = '',
      Constant = '󰏿',
      Struct = '',
      Event = '',
      Operator = '󰆕',
      TypeParameter = '󰅲',
    }

    lspkind.setup {
      mode = 'symbol_text',
      ellipsis_char = '...',
      show_labelDetails = true,
      symbols = kind_icons,
      fontName = 'Nerd Font',
      size = 14,
    }

    luasnip.config.setup {}

    cmp.setup {
      snippet = {
        expand = function(args)
          luasnip.lsp_expand(args.body)
        end,
      },
      window = {
        completion = cmp.config.window.bordered(),
        documentation = cmp.config.window.bordered(),
        -- completion = {
        --   cmp.config.window.bordered(),
        --   winhighlight = 'Normal:Pmenu,FloatBorder:Pmenu,Search:None',
        --   col_offset = -3,
        --   side_padding = 0,
        -- },
      },

      completion = { completeopt = 'menu,menuone,noinsert' },

      mapping = cmp.mapping.preset.insert {
        -- Select the [n]ext item
        -- ['<C-n>'] = cmp.mapping.select_next_item(),
        -- Select the [p]revious item
        -- ['<C-p>'] = cmp.mapping.select_prev_item(),

        -- Scroll the documentation window [b]ack / [f]orward
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),

        -- Accept ([y]es) the completion.
        --  This will auto-import if your LSP supports it.
        --  This will expand snippets if the LSP sent a snippet.
        -- ['<C-y>'] = cmp.mapping.confirm { select = true },

        -- If you prefer more traditional completion keymaps,
        -- you can uncomment the following lines
        ['<CR>'] = cmp.mapping.confirm { select = true },
        ['<Tab>'] = cmp.mapping.select_next_item(),
        ['<S-Tab>'] = cmp.mapping.select_prev_item(),

        -- Manually trigger a completion from nvim-cmp.
        --  Generally you don't need this, because nvim-cmp will display
        --  completions whenever it has completion options available.
        ['<C-Space>'] = cmp.mapping.complete {},

        -- Think of <c-l> as moving to the right of your snippet expansion.
        --  So if you have a snippet that's like:
        --  function $name($args)
        --    $body
        --  end
        --
        -- <c-l> will move you to the right of each of the expansion locations.
        -- <c-h> is similar, except moving you backwards.
        ['<C-l>'] = cmp.mapping(function()
          if luasnip.expand_or_locally_jumpable() then
            luasnip.expand_or_jump()
          end
        end, { 'i', 's' }),
        ['<C-h>'] = cmp.mapping(function()
          if luasnip.locally_jumpable(-1) then
            luasnip.jump(-1)
          end
        end, { 'i', 's' }),
      },
      sources = {
        {
          name = 'lazydev',
          group_index = 0,
        },
        { name = 'nvim_lsp' },
        { name = 'luasnip' },
        { name = 'path' },
        { name = 'vim-dadbod-completion' },
        { name = 'buffer' },
      },
      formatting = {
        fields = { 'kind', 'abbr', 'menu' },
        format = function(entry, vim_item)
          local kind = require('lspkind').cmp_format { mode = 'symbol_text', maxwidth = 50 } (entry, vim_item)
          local strings = vim.split(kind.kind, '%s', { trimempty = true })
          kind.kind = ' ' .. (strings[1] or '') .. ' '
          kind.menu = '    (' .. (strings[2] or '') .. ')'

          return kind
        end,
      },
    }
  end,
}
