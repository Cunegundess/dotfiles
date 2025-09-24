return {
  name = 'tsserver',
  cmd = { 'typescript-language-server', '--stdio' },
  root_dir = vim.fs.root(0, { 'package.json', 'tsconfig.json', 'jsconfig.json', '.git' }),
  filetypes = { 'typescript', 'typescriptreact', 'javascript', 'javascriptreact' },
  settings = {
    completions = {
      completeFunctionCalls = true,
    },
  },
}
