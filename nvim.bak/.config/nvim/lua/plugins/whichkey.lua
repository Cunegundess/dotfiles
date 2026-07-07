require('which-key').setup {
  preset = 'helix',
  delay = 0,
  plugins = {
    spelling = { enabled = true },
    presets = {
      operators = true,
      motions = true,
      text_objects = true,
      windows = true,
      nav = true,
      z = true,
      g = true,
    },
  },
}
