return {
  name = 'dartls',
  cmd = {
    'dart',
    vim.fn.expand '~' .. '/flutter/bin/cache/dart-sdk/bin/snapshots/analysis_server.dart.snapshot',
    '--lsp',
  },
  filetypes = { 'dart' },
  root_markers = {
    'pubspec.yaml',
    '.git',
  },
  init_options = {
    onlyAnalyzeProjectsWithOpenFiles = false,
    suggestFromUnimportedLibraries = true,
    closingLabels = true,
    outline = true,
    flutterOutline = true,
  },
  settings = {
    dart = {
      completeFunctionCalls = true,
      showTodos = true,
      enableSdkFormatter = true,
      enableSnippets = true,
      updateImportsOnRename = true,

      -- Otimizações para performance
      analysisExcludedFolders = {
        vim.fn.expand '~' .. '/flutter/bin/cache',
        '/build/',
        '**/.dart_tool/**',
      },

      -- Configurações do Flutter
      flutter = {
        sdkPath = vim.fn.expand '~' .. '/flutter',
        showInspectorOnDebug = true,
      },

      -- Linhas de fechamento de widgets
      closingLabels = true,

      -- Habilita análise null-safety
      strictInference = true,
    },
  },

  -- Capabilities específicas para Dart
  capabilities = {
    textDocument = {
      completion = {
        completionItem = {
          snippetSupport = true,
          commitCharactersSupport = true,
          deprecatedSupport = true,
          preselectSupport = true,
          tagSupport = {
            valueSet = { 1 },
          },
          insertReplaceSupport = true,
          resolveSupport = {
            properties = {
              'documentation',
              'detail',
              'additionalTextEdits',
            },
          },
          insertTextModeSupport = {
            valueSet = { 1, 2 },
          },
        },
        contextSupport = true,
      },
      signatureHelp = {
        signatureInformation = {
          parameterInformation = {
            labelOffsetSupport = true,
          },
          activeParameterSupport = true,
        },
      },
    },
    workspace = {
      configuration = true,
      workspaceFolders = true,
    },
  },

  -- Handlers para melhor experiência
  handlers = {
    ['textDocument/publishDiagnostics'] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
      virtual_text = {
        prefix = '●',
        spacing = 4,
      },
      signs = true,
      update_in_insert = false,
      underline = true,
    }),
  },

  -- On attach específico para Flutter/Dart
  on_attach = function(client, bufnr)
    -- Habilita formatação no save
    if client.server_capabilities.documentFormattingProvider then
      vim.api.nvim_create_autocmd('BufWritePre', {
        buffer = bufnr,
        callback = function()
          vim.lsp.buf.format {
            async = false,
            filter = function(c)
              return c.name == 'dartls'
            end,
          }
        end,
      })
    end

    -- Debug específico para Dart
    local group = vim.api.nvim_create_augroup('dart_lsp', { clear = true })
    vim.api.nvim_create_autocmd('CursorHold', {
      buffer = bufnr,
      callback = function()
        vim.diagnostic.open_float(nil, {
          focusable = false,
          close_events = { 'CursorMoved', 'CursorMovedI', 'BufHidden', 'InsertCharPre', 'WinLeave' },
          border = 'rounded',
          source = 'always',
          prefix = ' ',
          scope = 'cursor',
        })
      end,
      group = group,
    })
  end,
}
