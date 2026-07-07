require('mini.icons').setup()
vim.defer_fn(MiniIcons.mock_nvim_web_devicons, 0)

require('mini.ai').setup({
  n_lines = 500,
  mappings = { around = 'a', inside = 'i', around_next = 'an', inside_next = 'in' },
})

require('mini.surround').setup()
require('mini.pairs').setup({ modes = { command = true } })
require('mini.comment').setup()
require('mini.indentscope').setup({
  symbol = '│',
  draw = { animation = require('mini.indentscope').gen_animation.none() },
})
require('mini.bracketed').setup({
  comment = { suffix = '' }, file = { suffix = '' },
  indent = { suffix = '' }, location = { suffix = '' },
})
require('mini.move').setup({ lines = true, indent = true })
require('mini.splitjoin').setup()
require('mini.bufremove').setup()
require('mini.jump').setup()
require('mini.jump2d').setup()
require('mini.trailspace').setup()
require('mini.tabline').setup()
require('mini.animate').setup()
