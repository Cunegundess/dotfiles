return {
  name = 'pyright',
  cmd = { 'pyright-langserver', '--stdio' },
  filetypes = { 'python' },
  root_markers = {
    'pyproject.toml',
    'setup.py',
    'setup.cfg',
    'requirements.txt',
    'Pipfile',
    'pyrightconfig.json',
    '.git',
    'venv',
    'env'
  },
  settings = {
    pyright = {
      -- Configurações de análise
      analysis = {
        typeCheckingMode = 'basic',
        autoSearchPaths = true,
        useLibraryCodeForTypes = true,
        diagnosticMode = 'workspace',

        -- Configurações importantes para autoimport
        autoImportCompletions = true,
        importFormat = 'absolute', -- ou 'relative' dependendo da sua preferência

        -- Redução de falsos positivos
        reportMissingTypeStubs = false,
        reportOptionalMemberAccess = false,
        reportUnknownMemberType = false,
        reportUnknownArgumentType = false,
        reportUnknownVariableType = false,
        reportUnknownParameterType = false,

        -- Análise mais abrangente
        extraPaths = {},
        stubPath = 'typings',
      },

      -- Configurações de completions
      completion = {
        -- Habilita autoimport nas completions
        autoImport = true,
      },
    },
  },

  -- Habilita capabilities específicas para melhor funcionamento
  capabilities = {
    textDocument = {
      completion = {
        completionItem = {
          snippetSupport = true,
          resolveSupport = {
            properties = {
              'documentation',
              'detail',
              'additionalTextEdits',
            },
          },
        },
      },
    },
  },

  -- Configuração de handlers para melhor experiência
  handlers = {
    ['textDocument/publishDiagnostics'] = vim.lsp.with(
      vim.lsp.diagnostic.on_publish_diagnostics, {
        virtual_text = {
          prefix = '●',
          spacing = 4,
        },
        signs = true,
        update_in_insert = false,
      }
    ),
  },

  -- On attach específico se necessário
  on_attach = function(client, bufnr)
    -- Habilita formatação se disponível
    if client.server_capabilities.documentFormattingProvider then
      vim.api.nvim_create_autocmd('BufWritePre', {
        buffer = bufnr,
        callback = function()
          vim.lsp.buf.format({ async = false })
        end,
      })
    end
  end,
}
