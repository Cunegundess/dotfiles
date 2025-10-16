-- if true then
--   return {}
-- end

return {
  'folke/which-key.nvim',
  event = 'VimEnter',
  lazy = false,
  priority = 1000,
  config = function()
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
      show_help = true,
      show_keys = true,
    }
    --
    -- require('which-key').add {
    --   { '<leader>c', group = '[C]ode' },
    --   { '<leader>b', group = '[B]reakpoint' },
    --   { '<leader>d', group = '[D]ocument' },
    --   { '<leader>r', group = '[R]ename' },
    --   { '<leader>s', group = '[S]earch' },
    --   { '<leader>w', group = '[W]orkspace' },
    --   { '<leader>t', group = '[T]oggle' },
    --   { '<leader>h', group = 'Git [H]unk', mode = { 'n', 'v' } },
    -- }
  end,
}
