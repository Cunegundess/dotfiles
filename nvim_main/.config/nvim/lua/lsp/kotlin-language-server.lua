return {
  cmd = { 'kotlin-language-server' },
  filetypes = { 'kotlin' },
  root_markers = { 'settings.gradle', 'settings.gradle.kts', 'build.gradle', 'build.gradle.kts', '.git' },
  cmd_env = {
    JAVA_HOME = '/usr/lib64/jvm/java-21-openjdk-21',
  },
  on_attach = function(client, bufnr)
    client.server_capabilities.documentFormattingProvider = false
  end,
}
