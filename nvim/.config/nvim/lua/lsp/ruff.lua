return {
  cmd = { 'ruff', 'server' },
  root_dir = vim.fn.getcwd(),
  filetypes = { 'python' },
  init_options = {
    settings = {
      codeAction = {
        fixViolation = { enable = true },
        disableRuleComment = { enable = true },
      },
      organizeImports = true,
      fixAll = true,
      logLevel = 'info',
      logFile = nil,
      configuration = nil,
      configurationPreference = 'filesystemFirst',
      lint = {
        enable = true,
        select = { 'E', 'F', 'W' },
        ignore = { 'E501' },
        preview = true,
      },
      format = { preview = true },
    },
  },
}
