local keymap = vim.keymap.set

keymap('n', '<Esc>', '<cmd>nohlsearch<CR>', { desc = 'Clear search highlights' })
keymap('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })

keymap('n', '<left>', '<cmd>echo "Use h to move!!"<CR>')
keymap('n', '<right>', '<cmd>echo "Use l to move!!"<CR>')
keymap('n', '<up>', '<cmd>echo "Use k to move!!"<CR>')
keymap('n', '<down>', '<cmd>echo "Use j to move!!"<CR>')

keymap('n', '<leader>e', '<cmd>Otree<CR>', { desc = 'Toggle file tree' })
keymap('n', '<leader>E', '<cmd>Explore!<CR>', { desc = 'Open Netrw' })

keymap('n', '<leader>r', [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]], { desc = 'Replace word under cursor' })

-- Picker (fzf-lua)
keymap('n', '<leader>ff', require('fzf-lua').files, { desc = 'Find files' })
keymap('n', '<leader>fw', require('fzf-lua').live_grep, { desc = 'Live grep' })
keymap('n', '<leader>fg', require('fzf-lua').grep_visual, { desc = 'Grep visual' })
keymap('n', '<leader>fh', require('fzf-lua').help_tags, { desc = 'Help tags' })
keymap('n', '<leader>fk', require('fzf-lua').keymaps, { desc = 'Keymaps' })
keymap('n', '<leader><leader>', require('fzf-lua').buffers, { desc = 'Buffers' })
keymap('n', '<leader>ft', function()
  require('fzf-lua').colorschemes({
    actions = {
      ['default'] = function(selected)
        local theme = selected[1]:gsub('%.vim$', '')
        require('theme').apply(theme)
      end,
    },
  })
end, { desc = 'Themes' })
keymap('n', '<leader>fr', require('fzf-lua').resume, { desc = 'Resume picker' })

-- LSP
keymap('n', 'gd', require('fzf-lua').lsp_definitions, { desc = 'Go to definition' })
keymap('n', 'gr', require('fzf-lua').lsp_references, { desc = 'Go to references' })
keymap('n', 'gI', require('fzf-lua').lsp_implementations, { desc = 'Go to implementation' })
keymap('n', 'gD', require('fzf-lua').lsp_declarations, { desc = 'Go to declaration' })
keymap('n', '<leader>ca', require('fzf-lua').lsp_code_actions, { desc = 'Code actions' })
keymap('n', '<leader>D', require('fzf-lua').lsp_typedefs, { desc = 'Type definition' })
keymap('n', '<leader>fd', require('fzf-lua').lsp_document_diagnostics, { desc = 'Document diagnostics' })
keymap('n', '<leader>fs', require('fzf-lua').lsp_document_symbols, { desc = 'Document symbols' })
keymap('n', '<leader>li', '<cmd>lua vim.lsp.buf.hover()<CR>', { desc = 'Hover' })
keymap('n', '<leader>lr', '<cmd>lua vim.lsp.buf.rename()<CR>', { desc = 'Rename' })
keymap('n', '<leader>la', '<cmd>lua vim.lsp.buf.code_action()<CR>', { desc = 'Code actions' })

-- DAP
keymap('n', '<leader>bc', require('dap').continue, { desc = 'Debug continue' })
keymap('n', '<leader>bi', require('dap').step_into, { desc = 'Debug step into' })
keymap('n', '<leader>bo', require('dap').step_over, { desc = 'Debug step over' })
keymap('n', '<leader>bO', require('dap').step_out, { desc = 'Debug step out' })
keymap('n', '<leader>bb', require('dap').toggle_breakpoint, { desc = 'Toggle breakpoint' })
keymap('n', '<leader>bt', function()
  require('dapui').toggle()
end, { desc = 'Toggle DAP UI' })
keymap('n', '<leader>?', function()
  require('dapui').eval(nil, { enter = true })
end, { desc = 'Debug eval' })

-- Git
keymap('n', '<leader>gs', require('fzf-lua').git_status, { desc = 'Git status' })
keymap('n', '<leader>gc', require('fzf-lua').git_commits, { desc = 'Git commits' })
keymap('n', '<leader>gb', require('fzf-lua').git_branches, { desc = 'Git branches' })
keymap('n', '<leader>gB', require('fzf-lua').git_blame, { desc = 'Git blame' })
keymap('n', '<leader>hn', function()
  require('gitsigns').nav_hunk('next')
end, { desc = 'Next hunk' })
keymap('n', '<leader>hp', function()
  require('gitsigns').nav_hunk('prev')
end, { desc = 'Prev hunk' })
keymap('n', '<leader>hs', function()
  require('gitsigns').stage_hunk()
end, { desc = 'Stage hunk' })
keymap('n', '<leader>hr', function()
  require('gitsigns').reset_hunk()
end, { desc = 'Reset hunk' })
keymap('n', '<leader>hb', function()
  require('gitsigns').blame_line()
end, { desc = 'Blame line' })
keymap('n', '<leader>hd', function()
  require('gitsigns').diffthis()
end, { desc = 'Diff hunk' })
