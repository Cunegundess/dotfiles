-- Neo-tree is a Neovim plugin to browse the file system
-- https://github.com/nvim-neo-tree/neo-tree.nvim

return {
  'nvim-neo-tree/neo-tree.nvim',
  version = '*',
  dependencies = {
    'nvim-lua/plenary.nvim',
    'nvim-tree/nvim-web-devicons', -- not strictly required, but recommended
    'MunifTanjim/nui.nvim',
  },
  cmd = 'Neotree',
  keys = {
    { '<leader>E', ':Neotree reveal float<CR>', desc = 'NeoTree reveal', silent = true },
  },
  opts = {
    popup_border_style = '',
    filesystem = {
      filtered_items = {
        visible = true,
        hide_dotfiles = false,
        hide_gitignored = false,
      },
      window = {
        mappings = {
          ['<leader>E'] = 'close_window',
          ['P'] = {
            'toggle_preview',
            config = {
              use_float = true,
            },
            desc = 'Toggle preview',
          },
        },
      },
    },
  },
}
