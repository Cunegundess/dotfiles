return {
  cmd = { 'pylsp' },
  filetypes = { 'python' },
  settings = {
    pylsp = {
      plugins = {
        pycodestyle = { enabled = false },
        pylint = { enabled = false },
        pyflakes = { enabled = false },
        mccabe = { enabled = false },
        rope = { enabled = false },
        rope_completion = { enabled = false },
        yapf = { enabled = false },
        autopep8 = { enabled = false },
        black = { enabled = false },
        isort = { enabled = false },
        jedi_completion = { enabled = true },
        jedi_hover = { enabled = true },
        pylsp_mypy = {
          enabled = true,
          live_mode = true,
        },
      },
    },
  },
}
