return {
  cmd = { 'pyright-langserver', '--stdio' },
  filetypes = { 'python', 'py' },
  root_dir = vim.fn.getcwd(),
  root_markers = {
    'pyproject.toml',
    'setup.py',
    'setup.cfg',
    'requirements.txt',
    'Pipfile',
    'pyrightconfig.json',
    '.git',
  },
  settings = {
    python = {
      pythonPath = vim.fn.getcwd() .. '/venv/bin/python',
      analysis = {
        typeCheckingMode = 'basic',
        autoSearchPaths = true,
        useLibraryCodeForTypes = true,
        diagnosticMode = 'workspace',
        reportMissingTypeStubs = false,
        reportAttributeAccessIssue = false,
        reportOptionalMemberAccess = false,
        reportUnknownMemberType = false,
        reportUnknownArgumentType = false,
        reportUnknownVariableType = false,
        reportUnknownParameterType = false,
      },
    },
  },
}
