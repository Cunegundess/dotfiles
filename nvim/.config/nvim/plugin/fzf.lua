local fzf = require('fzf-lua')
fzf.setup({
  fzf_colors = true,
  winopts = {
    height = 0.9,
    width = 0.9,
    backdrop = 60,
    preview = { layout = 'vertical', vertical = 'down:70%' },
  },
  previewers = {
    cat = { cmd = 'cat', args = '-n' },
    bat = { cmd = 'bat', args = '--color=always --style=numbers,changes' },
  },
  files = {
    fd_opts = '--color=never --type f --hidden --follow '
      .. '--exclude .git --exclude node_modules --exclude venv '
      .. '--exclude .venv --exclude __pycache__ --exclude media '
      .. '--exclude data --exclude staticfiles',
  },
  grep = {
    rg_opts = '--column --line-number --no-heading --color=always '
      .. '--smart-case --hidden '
      .. "--glob '!.git' --glob '!node_modules' --glob '!venv' "
      .. "--glob '!.venv' --glob '!__pycache__' --glob '!media' "
      .. "--glob '!data' --glob '!staticfiles'",
  },
})
fzf.register_ui_select()

local map = vim.keymap.set

map('n', '<leader>ff', fzf.files, { desc = '[S]earch [F]iles' })
map('n', '<leader>fw', fzf.live_grep, { desc = '[S]earch current [W]ord' })
map('n', '<leader>fg', fzf.grep_visual, { desc = '[S]earch by [G]rep' })
map('n', '<leader>fh', fzf.help_tags, { desc = '[S]earch [H]elp' })
map('n', '<leader>fk', fzf.keymaps, { desc = '[S]earch [K]eymaps' })
map('n', '<leader><leader>', fzf.buffers, { desc = '[ ] Find existing buffers' })
map('n', '<leader>fr', fzf.resume, { desc = 'Resume picker' })
map('n', '<leader>ft', function()
  fzf.colorschemes({
    actions = {
      ['default'] = function(selected)
        require('theme').apply(selected[1]:gsub('%.vim$', ''))
      end,
    },
  })
end, { desc = '[S]earch [T]hemes' })

map('n', 'gd', fzf.lsp_definitions, { desc = '[G]oto [D]efinition' })
map('n', 'gr', fzf.lsp_references, { desc = '[G]oto [R]eferences' })
map('n', 'gI', fzf.lsp_implementations, { desc = '[G]oto [I]mplementation' })
map('n', 'gD', fzf.lsp_declarations, { desc = '[G]oto [D]eclaration' })
map('n', '<leader>ca', fzf.lsp_code_actions, { desc = '[C]ode [A]ctions' })
map('n', '<leader>D', fzf.lsp_typedefs, { desc = 'Type [D]efinition' })
map('n', '<leader>fd', fzf.lsp_document_diagnostics, { desc = '[S]earch [D]iagnostics' })
map('n', '<leader>fs', fzf.lsp_document_symbols, { desc = '[S]earch [S]ymbols' })

map('n', '<leader>gs', fzf.git_status, { desc = '[G]it [S]tatus' })
map('n', '<leader>gc', fzf.git_commits, { desc = '[G]it [C]ommits' })
map('n', '<leader>gb', fzf.git_branches, { desc = '[G]it [B]ranches' })
map('n', '<leader>gB', fzf.git_blame, { desc = '[G]it [B]lame' })

map('n', '<M-/>', function()
  if vim.v.count == 0 then
    vim.cmd('FzfLua files')
    return
  end
  fzf.live_grep({ cmd = 'git grep --line-number --column --color=always -v "^[[:space:]]*$"' })
end)
vim.cmd([[
  nnoremap <silent>       <M-\> :FzfLua oldfiles<cr>
  nnoremap <silent><expr> <C-\> v:count ? 'mS:<C-U>FzfLua lines<CR>' : ':<C-U>FzfLua buffers<CR>'
  nnoremap <silent> g/    :FzfLua lines<cr>
  nnoremap <silent> gO    :FzfLua btags<cr>
  nnoremap <silent> gr/   :FzfLua tags<cr>
]])

map({ 'n', 'v', 'i' }, '<C-x><C-f>', function()
  pcall(fzf.complete_path)
end, { silent = true, desc = 'Fuzzy complete path' })
map({ 'i' }, '<C-x><C-f>', function()
  fzf.complete_file({ cmd = 'rg --files', winopts = { preview = { hidden = true } } })
end, { silent = true, desc = 'Fuzzy complete file' })

vim.defer_fn(function()
  require('theme').apply()
end, 0)
