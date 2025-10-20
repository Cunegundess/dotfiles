return {
  'echasnovski/mini.nvim',
  version = false,
  config = function()
    require('mini.ai').setup { n_lines = 500 }
    require('mini.surround').setup()
    require('mini.pairs').setup()
    require('mini.comment').setup()
    require('mini.indentscope').setup { delay = 50, symbol = '│' }
    require('mini.diff').setup()
    require('mini.animate').setup()
    require('mini.fuzzy').setup()
  end,
}
