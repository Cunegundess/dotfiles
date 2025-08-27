-- ~/.config/nvim/lua/lsp/python-lsp-server.lua
-- Configuração APENAS do pylsp - sem capabilities, on_attach, etc.

return {
  cmd = { 'pylsp' },
  filetypes = { 'python' },

  root_patterns = {
    'pyproject.toml',
    'setup.py',
    'setup.cfg',
    'requirements.txt',
    'Pipfile',
    'manage.py',
    '.git',
  },

  settings = {
    pylsp = {
      configurationSources = { 'pycodestyle' },

      plugins = {
        -- === ANÁLISE NATIVA ===
        pyflakes = {
          enabled = false,
        },

        pycodestyle = {
          enabled = false,
          ignore = { 'E501', 'W503', 'E203', 'W504' },
          maxLineLength = 88,
        },

        mccabe = {
          enabled = false,
          threshold = 15,
        },

        -- === JEDI (sempre disponível) ===
        jedi_completion = {
          enabled = true,
          fuzzy = true,
          eager = true,
          include_params = true,
          include_class_objects = true,
          include_function_objects = true,
        },

        jedi_hover = {
          enabled = true,
        },

        jedi_references = {
          enabled = true,
        },

        jedi_signature_help = {
          enabled = true,
        },

        jedi_symbols = {
          enabled = true,
          all_scopes = true,
          include_import_symbols = true,
        },

        jedi_definition = {
          enabled = true,
          follow_imports = true,
          follow_builtin_imports = true,
        },

        jedi_rename = {
          enabled = true,
        },

        -- === FORMATADORES NATIVOS ===
        autopep8 = {
          enabled = false,
        },

        yapf = {
          enabled = false,
        },

        -- === PLUGINS EXTERNOS (se instalados) ===

        -- MyPy (pylsp-mypy)
        pylsp_mypy = {
          enabled = false,
          live_mode = true,
          strict = false,
          report_progress = true,
          dmypy = false,
          overrides = {
            '--follow-imports=silent',
            true,
          },
        },

        -- Rope básico (pylsp-rope)
        rope = {
          enabled = false,
          ropeFolder = { '.ropeproject' },
        },

        -- Rope plugins específicos (podem não existir)
        rope_completion = {
          enabled = false, -- Desabilitado por segurança
        },

        rope_rename = {
          enabled = false, -- Desabilitado por segurança
        },

        rope_autoimport = {
          enabled = false, -- Desabilitado por segurança
        },

        -- Ruff
        ruff = {
          enabled = true,
          format = { 'I' },
          select = { 'E', 'F', 'W' },
          ignore = { 'E501' },
          preview = true,
        },

        -- isort (formatação de imports)
        isort = {
          enabled = true,
        },
      },
    },
  },
}
