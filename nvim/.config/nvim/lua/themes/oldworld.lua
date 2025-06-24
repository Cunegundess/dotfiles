return {
  'dgox16/oldworld.nvim',
  lazy = false,
  priority = 1000,
  opts = {
    variant = 'default', -- default, oled, cooler
    integrations = { -- You can disable/enable integrations
      neo_tree = true,
      neogit = true,
    },
  },
}
