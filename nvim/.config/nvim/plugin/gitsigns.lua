local ok, gitsigns = pcall(require, 'gitsigns')
if not ok then return end

gitsigns.setup({
  signs = {
    add = { text = '┃' }, change = { text = '┃' }, delete = { text = '_' },
    topdelete = { text = '‾' }, changedelete = { text = '~' }, untracked = { text = '┆' },
  },
  current_line_blame = true,
  current_line_blame_opts = { virt_text_pos = 'eol_right_align' },
  signs_staged_enable = false,
})

local map = vim.keymap.set

map('n', '<leader>hn', function() gitsigns.nav_hunk('next') end, { desc = 'Next hunk' })
map('n', '<leader>hp', function() gitsigns.nav_hunk('prev') end, { desc = 'Prev hunk' })
map('n', '<leader>hs', function() gitsigns.stage_hunk() end, { desc = 'Stage hunk' })
map('n', '<leader>hr', function() gitsigns.reset_hunk() end, { desc = 'Reset hunk' })
map('n', '<leader>hb', function() gitsigns.blame_line() end, { desc = 'Blame line' })
map('n', '<leader>hd', function() gitsigns.diffthis() end, { desc = 'Diff hunk' })
