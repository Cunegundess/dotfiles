local mini = {
  'mini.ai',
  'mini.surround',
  'mini.pairs',
  'mini.comment',
  'mini.indentscope',
  'mini.bracketed',
  'mini.move',
  'mini.splitjoin',
}

for _, mod in ipairs(mini) do
  pcall(require, mod)
end

require('mini.ai').setup {
  n_lines = 500,
  mappings = {
    around = 'a',
    inside = 'i',
    around_next = 'an',
    inside_next = 'in',
    around_last = 'al',
    inside_last = 'il',
  },
}

require('mini.surround').setup()

require('mini.pairs').setup()

require('mini.comment').setup {
  hooks = {
    pre = function()
      require('mini.comment').toggle()
    end,
  },
}

require('mini.indentscope').setup {
  symbol = '│',
  options = { try_as_border = true },
}

require('mini.bracketed').setup()

require('mini.move').setup {
  lines = true,
  indent = true,
}

require('mini.splitjoin').setup()
