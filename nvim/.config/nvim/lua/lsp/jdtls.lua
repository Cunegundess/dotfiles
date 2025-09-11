-- Configuração para jdtls
return {
  name = 'jdtls',
  cmd = { '/run/current-system/sw/bin/jdtls' },
  filetypes = { 'java' },
  root_markers = {
    'pom.xml',
    'gradlew',
    'build.gradle',
    '.git',
    'mvnw'
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
          "org.junit.Assert.*",
          "org.junit.jupiter.api.Assertions.*",
          "org.mockito.Mockito.*",
          "org.mockito.ArgumentMatchers.*",
          "org.hamcrest.MatcherAssert.*",
          "org.hamcrest.Matchers.*",
        }
      },
      sources = {
        organizeImports = {
          starThreshold = 9999,
          staticStarThreshold = 9999,
        },
      },
      codeGeneration = {
        toString = {
          template = "${object.className}{${member.name()}=${member.value}, ${otherMembers}}"
        },
        useBlocks = true,
      },
    },
  },
  init_options = {
    bundles = {}
  },
}
