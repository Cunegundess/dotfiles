-- mini.icons
require('mini.icons').setup()
vim.defer_fn(MiniIcons.mock_nvim_web_devicons, 0)

-- mini.ai (textobjects)
require('mini.ai').setup({
  n_lines = 500,
  mappings = {
    around = 'a',
    inside = 'i',
    around_next = 'an',
    inside_next = 'in',
  },
})

-- mini.surround
require('mini.surround').setup()

-- mini.pairs (autopairs)
require('mini.pairs').setup({ modes = { command = true } })

-- mini.comment
require('mini.comment').setup()

-- mini.indentscope
require('mini.indentscope').setup({
  symbol = '│',
  draw = { animation = require('mini.indentscope').gen_animation.none() },
})

-- mini.bracketed (navegação com [ ])
require('mini.bracketed').setup({
  comment = { suffix = '' },
  file = { suffix = '' },
  indent = { suffix = '' },
  location = { suffix = '' },
})

-- mini.move
require('mini.move').setup({ lines = true, indent = true })

-- mini.splitjoin
require('mini.splitjoin').setup()

-- mini.bufremove (buffer delete/wipeout)
require('mini.bufremove').setup()

-- mini.jump (f/t smarter)
require('mini.jump').setup()

-- mini.jump2d (jump to visible text)
require('mini.jump2d').setup()

-- mini.trailspace (trailing whitespace)
require('mini.trailspace').setup()

-- mini.tabline
require('mini.tabline').setup()

-- mini.animate
require('mini.animate').setup()
