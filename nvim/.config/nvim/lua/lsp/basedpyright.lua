return {
  name = 'basedpyright',
  cmd = { 'basedpyright-langserver', '--stdio' },
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
    'env',
  },

  settings = {
    basedpyright = {
      -- Configurações gerais do servidor
      disableLanguageServices = false, -- Desabilita hover, completion, etc
      disableOrganizeImports = false, -- Desabilita "Organize Imports"
      disableTaggedHints = false, -- Desabilita hints de código inalcançável/deprecado

      analysis = {
        -- Modo de diagnóstico: "openFilesOnly" ou "workspace"
        diagnosticMode = 'workspace',

        -- Nível de verificação: "off", "basic", "standard", "strict", "recommended", "all"
        typeCheckingMode = 'strict',

        -- Configurações de auto-importação e paths
        autoImportCompletions = true, -- Sugestões de auto-import
        autoSearchPaths = true, -- Adiciona "src" automaticamente
        useLibraryCodeForTypes = true, -- Analisa código de bibliotecas

        -- Log level: "Error", "Warning", "Information", "Trace"
        logLevel = 'Information',

        -- ===== CONFIGURAÇÕES EXCLUSIVAS DO BASEDPYRIGHT =====

        -- Inlay hints (dicas visuais inline)
        inlayHints = {
          variableTypes = true, -- Mostra tipos em variáveis
          callArgumentNames = true, -- Mostra nomes de argumentos
          callArgumentNamesMatching = false, -- Mostra quando nome coincide
          functionReturnTypes = true, -- Mostra tipos de retorno
          genericTypes = false, -- Mostra tipos genéricos inferidos
        },

        -- Outras configurações exclusivas
        useTypingExtensions = false, -- Usa typing_extensions para versões antigas
        fileEnumerationTimeout = 10, -- Timeout para scan de arquivos (segundos)
        autoFormatStrings = true, -- Auto-insere 'f' em f-strings

        -- Baseline file (ignorar diagnósticos conhecidos)
        -- baselineFile = ".basedpyright/baseline.json",

        -- ===== CONFIGURAÇÕES DESENCORAJADAS (use pyproject.toml) =====
        -- Recomenda-se configurar estas no pyproject.toml ou pyrightconfig.json
        -- Deixo aqui como exemplo, mas comente se usar arquivo de config

        -- Severidade de diagnósticos específicos
        diagnosticSeverityOverrides = {
          reportUnusedImport = 'warning',
          reportUnusedVariable = 'warning',
          reportDuplicateImport = 'warning',
          reportUnusedClass = 'information',
          reportUnusedFunction = 'information',
          reportMissingTypeStubs = 'none',
          reportImportCycles = 'warning',
          reportPrivateUsage = 'warning',
          reportConstantRedefinition = 'error',
          reportIncompatibleMethodOverride = 'error',
          reportIncompatibleVariableOverride = 'error',
          reportUntypedFunctionDecorator = 'none',
          reportUntypedClassDecorator = 'none',
          reportUntypedBaseClass = 'none',
          reportUnknownParameterType = 'none',
          reportUnknownArgumentType = 'none',
          reportUnknownLambdaType = 'none',
          reportUnknownVariableType = 'none',
          reportUnknownMemberType = 'none',
          reportMissingParameterType = 'none',
          reportMissingTypeArgument = 'none',
        },

        -- Paths extras
        extraPaths = {},

        -- Stub path
        -- stubPath = 'typings',

        -- Exclusões e inclusões
        exclude = {
          '**/node_modules',
          '**/__pycache__',
          '.venv',
          'venv',
        },

        ignore = {}, -- Arquivos para suprimir diagnósticos
        include = {}, -- Arquivos para incluir
      },
    },

    -- Configurações do Python (se necessário)
    python = {
      -- pythonPath = ".venv/bin/python",  -- Path do interpretador Python
      -- venvPath = ".",  -- Path para virtual environments (desencorajado)
    },
  },

  -- Capacidades do cliente
  capabilities = vim.lsp.protocol.make_client_capabilities(),
}
