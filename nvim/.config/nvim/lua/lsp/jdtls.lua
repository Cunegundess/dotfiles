-- Configuração para jdtls
local jdtls_path = vim.fn.stdpath 'data' .. '/mason/packages/jdtls'
local launcher_path = vim.fn.glob(jdtls_path .. '/plugins/org.eclipse.equinox.launcher_*.jar')
local config_path = jdtls_path .. '/config_linux'
local workspace = vim.fn.stdpath 'cache' .. '/jdtls-workspace/' .. vim.fn.fnamemodify(vim.fn.getcwd(), ':p:h:t')

return {
  name = 'jdtls',
  cmd = {
    'java',
    '-Declipse.application=org.eclipse.jdt.ls.core.id1',
    '-Dosgi.bundles.defaultStartLevel=4',
    '-Declipse.product=org.eclipse.jdt.ls.core.product',
    '-Dlog.protocol=true',
    '-Dlog.level=ALL',
    '-Xms1g',
    '--add-modules=ALL-SYSTEM',
    '--add-opens',
    'java.base/java.util=ALL-UNNAMED',
    '--add-opens',
    'java.base/java.lang=ALL-UNNAMED',
    '-jar',
    launcher_path,
    '-configuration',
    config_path,
    '-data',
    workspace,
  },
  filetypes = { 'java' },
  root_markers = {
    'pom.xml',
    'gradlew',
    'build.gradle',
    '.git',
    'mvnw',
  },
  settings = {
    java = {
      -- configuration = {
      --   runtimes = {
      --     {
      --       name = "JavaSE-17",
      --       path = "/nix/store/*-jdk-17*/",
      --     },
      --     {
      --       name = "JavaSE-21",
      --       path = "/nix/store/*-jdk-21*/",
      --     }
      --   }
      -- },
      signatureHelp = { enabled = true },
      contentProvider = { preferred = 'fernflower' },
      completion = {
        favoriteStaticMembers = {
          'org.junit.Assert.*',
          'org.junit.jupiter.api.Assertions.*',
          'org.mockito.Mockito.*',
          'org.mockito.ArgumentMatchers.*',
          'org.hamcrest.MatcherAssert.*',
          'org.hamcrest.Matchers.*',
        },
      },
      sources = {
        organizeImports = {
          starThreshold = 9999,
          staticStarThreshold = 9999,
        },
      },
      codeGeneration = {
        toString = {
          template = '${object.className}{${member.name()}=${member.value}, ${otherMembers}}',
        },
        useBlocks = true,
      },
    },
  },
  init_options = {
    bundles = {},
  },
}
