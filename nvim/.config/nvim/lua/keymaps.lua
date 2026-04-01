local keymap = vim.keymap.set

keymap('n', '<leader>ca', vim.lsp.buf.code_action, { desc = '[C]ode [A]ctions' })
keymap('n', '<leader>e', '<cmd>Explore!<CR>', { desc = 'Open Netrw' })
keymap('n', '<Esc>', '<cmd>nohlsearch<CR>', { desc = 'Clear search highlights' })
keymap('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostic [Q]uickfix' })
keymap('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })

keymap('n', '<left>', '<cmd>echo "Use h to move!!"<CR>', { desc = 'Disable Left Arrow' })
keymap('n', '<right>', '<cmd>echo "Use l to move!!"<CR>', { desc = 'Disable Right Arrow' })
keymap('n', '<up>', '<cmd>echo "Use k to move!!"<CR>', { desc = 'Disable Up Arrow' })
keymap('n', '<down>', '<cmd>echo "Use j to move!!"<CR>', { desc = 'Disable Down Arrow' })

keymap('n', '<C-h>', '<C-w><C-h>', { desc = 'Move focus to the left window' })
keymap('n', '<C-l>', '<C-w><C-l>', { desc = 'Move focus to the right window' })
keymap('n', '<C-j>', '<C-w><C-j>', { desc = 'Move focus to the lower window' })
keymap('n', '<C-k>', '<C-w><C-k>', { desc = 'Move focus to the upper window' })

keymap('n', '<M-l>', '<cmd>vertical resize -2<CR>', { desc = 'Resize split left' })
keymap('n', '<M-h>', '<cmd>vertical resize +2<CR>', { desc = 'Resize split right' })
keymap('n', '<M-k>', '<cmd>resize +2<CR>', { desc = 'Resize split down' })
keymap('n', '<M-j>', '<cmd>resize -2<CR>', { desc = 'Resize split up' })

keymap('v', 'J', ":m '>+1<CR>gv=gv", { desc = 'Move selection down' })
keymap('v', 'K', ":m '<-2<CR>gv=gv", { desc = 'Move selection up' })
keymap('n', 'J', 'mzJ`z', { desc = 'Join lines and keep cursor' })
keymap('n', '<leader>r', [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]], { desc = 'Replace word under cursor' })

local fzf_lua_ok, fzf_lua = pcall(require, 'fzf-lua')
if fzf_lua_ok then
  keymap('n', '<leader>ff', fzf_lua.files, { desc = '[S]earch [F]iles' })
  keymap('n', '<leader>fw', fzf_lua.live_grep, { desc = '[S]earch current [W]ord' })
  keymap('n', '<leader>fg', fzf_lua.grep_visual, { desc = '[S]earch by [G]rep' })
  keymap('n', '<leader>fh', fzf_lua.help_tags, { desc = '[S]earch [H]elp' })
  keymap('n', '<leader>fk', fzf_lua.keymaps, { desc = '[S]earch [K]eymaps' })
  keymap('n', '<leader><leader>', fzf_lua.buffers, { desc = '[ ] Find existing buffers' })
  keymap('n', '<leader>ft', fzf_lua.colorschemes, { desc = '[S]earch [T]hemes' })
  keymap('n', 'gd', fzf_lua.lsp_definitions, { desc = '[G]oto [D]efinition' })
  keymap('n', 'gr', fzf_lua.lsp_references, { desc = '[G]oto [R]eferences' })
  keymap('n', 'gI', fzf_lua.lsp_implementations, { desc = '[G]oto [I]mplementation' })
  keymap('n', '<leader>D', fzf_lua.lsp_typedefs, { desc = 'Type [D]efinition' })
  keymap('n', '<leader>fd', fzf_lua.lsp_document_diagnostics, { desc = '[S]earch [D]iagnostics' })
  keymap('n', '<leader>fs', fzf_lua.lsp_document_symbols, { desc = '[S]earch [S]ymbols' })
  keymap('n', '<leader>gD', fzf_lua.lsp_declarations, { desc = '[G]oto [D]eclaration' })
  keymap('n', '<leader>gs', fzf_lua.git_status, { desc = '[G]it [S]tatus' })
  keymap('n', '<leader>gc', fzf_lua.git_commits, { desc = '[G]it [C]ommits' })
  keymap('n', '<leader>gb', fzf_lua.git_branches, { desc = '[G]it [B]ranches' })
  keymap('n', '<leader>gB', fzf_lua.git_blame, { desc = '[G]it [B]lame' })
end

local gitsigns_ok, gitsigns = pcall(require, 'gitsigns')
if gitsigns_ok then
  keymap('n', ']c', function()
    if vim.wo.diff then vim.cmd.normal { ']c', bang = true }
    else gitsigns.nav_hunk 'next' end
  end, { desc = 'Jump to next git [c]hange' })
  keymap('n', '[c', function()
    if vim.wo.diff then vim.cmd.normal { '[c', bang = true }
    else gitsigns.nav_hunk 'prev' end
  end, { desc = 'Jump to previous git [c]hange' })
  keymap('n', '<leader>hs', gitsigns.stage_hunk, { desc = 'git [s]tage hunk' })
  keymap('n', '<leader>hr', gitsigns.reset_hunk, { desc = 'git [r]eset hunk' })
  keymap('n', '<leader>hS', gitsigns.stage_buffer, { desc = 'git [S]tage buffer' })
  keymap('n', '<leader>hu', gitsigns.undo_stage_hunk, { desc = 'git [u]ndo stage hunk' })
  keymap('n', '<leader>hR', gitsigns.reset_buffer, { desc = 'git [R]eset buffer' })
  keymap('n', '<leader>hp', gitsigns.preview_hunk, { desc = 'git [p]review hunk' })
  keymap('n', '<leader>hb', gitsigns.blame_line, { desc = 'git [b]lame line' })
  keymap('n', '<leader>hd', gitsigns.diffthis, { desc = 'git [d]iff against index' })
end

local harpoon_ok, harpoon = pcall(require, 'harpoon')
if harpoon_ok then
  keymap('n', '<leader>ha', harpoon.mark.add_file, { desc = 'Harpoon: Add file' })
  keymap('n', '<leader>ht', harpoon.ui.toggle_quick_menu, { desc = 'Harpoon: Toggle menu' })
  keymap('n', '<leader>hn', harpoon.ui.nav_next, { desc = 'Harpoon: Next file' })
  keymap('n', '<leader>hp', harpoon.ui.nav_prev, { desc = 'Harpoon: Previous file' })
end
