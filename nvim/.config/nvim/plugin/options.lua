vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
vim.g.have_nerd_font = true

vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.signcolumn = 'yes'
vim.opt.colorcolumn = ''
vim.opt.cursorline = true
vim.opt.wrap = false
vim.opt.linebreak = true
vim.opt.showcmd = true
vim.opt.cmdheight = 1
vim.opt.pumheight = 10
vim.opt.pumblend = 10
vim.o.winborder = 'rounded'
vim.o.mouse = 'a'

vim.o.shiftwidth = 2
vim.o.tabstop = 2
vim.o.softtabstop = -1
vim.o.expandtab = true
vim.o.autoindent = true
vim.o.smartindent = true
vim.opt.shiftround = true
vim.opt.copyindent = true

vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.inccommand = 'split'

vim.schedule(function()
  vim.opt.clipboard = 'unnamedplus'
end)

vim.opt.undofile = true
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.autoread = true

vim.opt.scrolloff = 2
vim.opt.sidescrolloff = 8
vim.opt.smoothscroll = true

vim.opt.updatetime = 250
vim.opt.timeoutlen = 3000
vim.opt.ttimeoutlen = 10

vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.equalalways = true

vim.opt.list = true
vim.opt.listchars = {
  tab = '· ',
  trail = '⣿',
  nbsp = '␣',
  extends = '❯',
  precedes = '❮',
}

vim.opt.completeopt = { 'menuone', 'noselect', 'noinsert', 'fuzzy', 'popup' }
vim.opt.complete = '.,w,b,k,kspell'
vim.opt.wildmenu = true
vim.opt.wildmode = 'longest:full,full'
vim.opt.wildoptions = 'pum,fuzzy'
vim.opt.wildcharm = 26

vim.opt.breakindent = true
vim.opt.confirm = true
vim.opt.hidden = true
vim.opt.selection = 'exclusive'
vim.opt.virtualedit = 'block'

vim.opt.formatoptions:remove({ 'c', 'r', 'o' })
vim.opt.iskeyword:append('-')
vim.opt.diffopt:append('hiddenoff,linematch:60,foldcolumn:0')
vim.opt.shortmess:append('cC')
vim.opt.laststatus = 3

vim.opt.foldlevel = 99
vim.opt.foldlevelstart = 99
vim.opt.foldmethod = 'indent'
vim.opt.foldnestmax = 10
vim.opt.foldopen:remove({ 'search' })

vim.opt.sessionoptions:remove({ 'blank' })
vim.opt.exrc = true
vim.opt.fillchars:append('eob:·,msgsep:‾')
vim.opt.title = true
vim.o.showmode = false
vim.o.startofline = false

vim.cmd('filetype plugin indent on')
if vim.fn.exists('syntax_on') ~= 1 then vim.cmd('syntax enable') end

local augroup = vim.api.nvim_create_augroup

vim.api.nvim_create_autocmd('TextYankPost', {
  group = augroup('highlight_yank', { clear = true }),
  pattern = '*',
  callback = function()
    vim.highlight.on_yank({ timeout = 200, visual = true })
  end,
})

vim.api.nvim_create_autocmd('FileType', {
  group = augroup('no_auto_comment', {}),
  callback = function()
    vim.opt_local.formatoptions:remove({ 'c', 'r', 'o' })
  end,
})

vim.api.nvim_create_autocmd({ 'WinEnter', 'BufEnter' }, {
  group = augroup('active_cursorline', { clear = true }),
  callback = function()
    vim.opt_local.cursorline = true
  end,
})

vim.api.nvim_create_autocmd({ 'WinLeave', 'BufLeave' }, {
  group = 'active_cursorline',
  callback = function()
    vim.opt_local.cursorline = false
  end,
})

vim.api.nvim_create_autocmd('CursorMoved', {
  group = augroup('LspReferenceHighlight', { clear = true }),
  callback = function()
    if vim.fn.mode() ~= 'i' then
      local clients = vim.lsp.get_clients({ bufnr = 0 })
      for _, client in ipairs(clients) do
        if client.server_capabilities.documentHighlightProvider then
          vim.lsp.buf.clear_references()
          vim.lsp.buf.document_highlight()
          return
        end
      end
    end
  end,
})

vim.api.nvim_create_autocmd('CursorMovedI', {
  group = 'LspReferenceHighlight',
  callback = function()
    vim.lsp.buf.clear_references()
  end,
})

vim.api.nvim_create_autocmd('FileType', {
  callback = function(ev)
    if ev.match and ev.match ~= '' and ev.match ~= 'text' then
      pcall(function() vim.treesitter.start(ev.buf) end)
    end
  end,
})

vim.api.nvim_create_autocmd('BufReadPost', {
  callback = function(args)
    local mark = vim.api.nvim_buf_get_mark(args.buf, '"')
    local line_count = vim.api.nvim_buf_line_count(args.buf)
    if mark[1] > 0 and mark[1] <= line_count then
      vim.api.nvim_win_set_cursor(0, mark)
      vim.schedule(function()
        vim.cmd('normal! zz')
      end)
    end
  end,
})
