vim.api.nvim_set_var('projectionist_heuristics', {
  ['package.json'] = {
    ['package.json'] = { alternate = { 'package-lock.json' } },
    ['package-lock.json'] = { alternate = { 'package.json' } },
  },
  ['*.sln'] = {
    ['*.cs'] = { alternate = { '{}.designer.cs' } },
    ['*.designer.cs'] = { alternate = { '{}.cs' } },
  },
  ['/*.c|src/*.c'] = {
    ['*.c'] = { alternate = { '../include/{}.h', '{}.h' } },
    ['*.h'] = { alternate = '{}.c' },
  },
  ['Makefile'] = {
    ['Makefile'] = { alternate = 'CMakeLists.txt' },
    ['CMakeLists.txt'] = { alternate = 'Makefile' },
  },
})
