require('lualine').setup {
  options = {
    theme = 'auto',
    component_separators = '|',
    section_separators = { left = '', right = '' },
  },
  sections = {
    lualine_a = { 'mode' },
    lualine_b = {},
    lualine_c = { { 'filename', path = 1 } },
    lualine_x = {
      { 'diagnostics', symbols = { error = ' ', warn = ' ', info = ' ' } },
      { 'diff', symbols = { added = ' ', modified = ' ', removed = ' ' } },
      'encoding',
      'fileformat',
      'filetype',
      'location',
    },
    lualine_y = {},
    lualine_z = {},
  },
}
