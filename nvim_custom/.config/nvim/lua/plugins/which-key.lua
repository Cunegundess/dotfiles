return {
  'folke/which-key.nvim',
  event = 'VimEnter',
  lazy = false,
  priority = 1000,
  config = function()
    require('which-key').setup {
      preset = 'helix',
      delay = 0,
    }

    require('which-key').add {
      { '<leader>c', group = '[C]ode' },
      { '<leader>b', group = '[B]reakpoint' },
      { '<leader>d', group = '[D]ocument' },
      { '<leader>r', group = '[R]ename' },
      { '<leader>s', group = '[S]earch' },
      { '<leader>w', group = '[W]orkspace' },
      { '<leader>t', group = '[T]oggle' },
      { '<leader>h', group = 'Git [H]unk', mode = { 'n', 'v' } },
    }
  end,
}
