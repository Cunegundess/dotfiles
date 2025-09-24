-- [[ Global Variables ]]
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
vim.g.have_nerd_font = true
vim.g.termguicolors = true

-- Disable netrw (use if you have a file explorer plugin)
-- vim.g.loaded_netrw = 1
-- vim.g.loaded_netrwPlugin = 1

-- [[ Setting options ]]
--
-- _G.git_branch = function()
--   local handle = io.popen 'git branch --show-current 2>/dev/null'
--   if handle then
--     local result = handle:read '*a' or ''
--     handle:close()
--     result = result:gsub('%s+', '') -- remove quebras de linha
--     if result ~= '' then
--       return ' ' .. result .. ' '
--     end
--   end
--   return ''
-- end
--
-- -- Define a statusline
-- vim.o.statusline = table.concat({
--   '%f', -- caminho/nome do arquivo
--   ' %m', -- indicador de modificado [+]
--   ' %r', -- indicador de somente leitura
--   ' %h', -- help file flag
--   ' %w', -- preview window flag
--   ' %=', -- separador (alinha a direita)
--   '%{v:lua.git_branch()}', -- branch git
--   ' %y', -- tipo de arquivo
--   ' %p%%', -- percentual no buffer
-- }, '')

-- Interface and Visual Settings
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.colorcolumn = '130' -- Visual guide at column 80
vim.opt.signcolumn = 'yes' -- Keep signcolumn on by default
vim.opt.cursorline = true -- Show which line your cursor is on
vim.opt.wrap = false -- Don't wrap long lines
vim.opt.linebreak = true -- Break at word boundaries if wrap is enabled
vim.opt.showmode = false -- Don't show the mode, since it's already in status line
vim.opt.showcmd = true -- Show partial command in status line
vim.opt.cmdheight = 1 -- Height of command line
vim.opt.laststatus = 3 -- Global statusline (Neovim 0.7+)
vim.opt.pumheight = 10 -- Maximum height of popup menu
vim.opt.pumblend = 10 -- Popup menu transparency

-- Indentation and Formatting
vim.o.shiftwidth = 2 -- Size of an indent
vim.o.tabstop = 2 -- Number of spaces tabs count for
vim.o.softtabstop = 2 -- Number of spaces tabs count for in insert mode
vim.o.expandtab = true -- Use spaces instead of tabs
vim.o.autoindent = true -- Insert indents automatically
vim.o.smartindent = true -- Insert indents automatically
vim.opt.shiftround = true -- Round indent to multiple of shiftwidth
vim.opt.copyindent = true -- Copy indent structure

-- Search and Replace
vim.opt.ignorecase = true -- Case-insensitive searching UNLESS \C or capital letters
vim.opt.smartcase = true -- Override ignorecase if search contains capitals
vim.opt.hlsearch = true -- Highlight search results
vim.opt.incsearch = true -- Incremental search
vim.opt.inccommand = 'split' -- Preview substitutions live, as you type!

-- Mouse and Input
vim.opt.mouse = 'a' -- Enable mouse mode, useful for resizing splits

-- Clipboard (with schedule to avoid issues)
vim.schedule(function()
  vim.opt.clipboard = 'unnamedplus' -- Sync clipboard between OS and Neovim
end)

-- Undo and Backup
vim.opt.undofile = true -- Save undo history
vim.opt.swapfile = false -- Disable swap files (they can be annoying)
vim.opt.backup = false -- Don't create backup files
vim.opt.writebackup = false -- Don't create temporary backup
vim.opt.autoread = true -- Reload file if changed outside of vim

-- Scrolling and Navigation
vim.opt.scrolloff = 10 -- Minimal number of screen lines above and below cursor
vim.opt.sidescrolloff = 8 -- Minimal number of columns to keep to the side of cursor
vim.opt.smoothscroll = true -- Smooth scrolling (Neovim 0.10+)

-- Timing
vim.opt.updatetime = 250 -- Decrease update time
vim.opt.timeoutlen = 300 -- Time to wait for mapped sequence (increased from your 100)
vim.opt.ttimeoutlen = 10 -- Time to wait for key code sequence

-- Splits
vim.opt.splitright = true -- Configure how new splits should be opened
vim.opt.splitbelow = true -- Configure how new splits should be opened
vim.opt.equalalways = true -- Make splits equal after resizing

-- Whitespace Display
vim.opt.list = true -- Show some invisible characters
vim.opt.listchars = {
  tab = '· ', -- Show tabs as arrows
  trail = '·', -- Show trailing spaces
  nbsp = '␣', -- Show non-breaking spaces
  extends = '❯', -- Show when line extends beyond screen
  precedes = '❮', -- Show when line precedes screen
}

-- -- Fill characters for various UI elements
-- vim.opt.fillchars = {
--   foldopen = '▾', -- ícone para fold aberto
--   foldclose = '▸', -- ícone para fold fechado
--   foldsep = ' ', -- separador (vazio)
--   fold = ' ', -- espaço dentro do fold
--   diff = '∙',
--   eob = ' ',
-- }
--
-- -- Folding
-- vim.opt.foldmethod = 'expr' -- Use expression for folding (bom com treesitter)
-- vim.opt.foldexpr = 'nvim_treesitter#foldexpr()' -- Treesitter para folding
-- vim.opt.foldlevel = 99 -- Começa com todos folds abertos
-- vim.opt.foldcolumn = '1' -- Remove os números de nível de fold
-- vim.opt.foldenable = true -- Ativa folding

-- Completion
vim.opt.completeopt = { 'menu', 'menuone', 'noselect' } -- Completion options
vim.opt.wildmenu = true -- Command line completion
vim.opt.wildmode = 'longest:full,full' -- Command line completion mode
vim.opt.wildoptions = 'pum' -- Use popup for wildmenu

-- Miscellaneous
vim.opt.breakindent = true -- Enable break indent
vim.opt.confirm = true -- Confirm before closing unsaved files
vim.opt.hidden = true -- Allow hidden buffers
vim.opt.selection = 'exclusive' -- Selection behavior
vim.opt.virtualedit = 'block' -- Allow virtual editing in visual block mode

-- Performance
vim.opt.lazyredraw = false -- Don't redraw while executing macros (can cause issues with modern plugins)
vim.opt.redrawtime = 1500 -- Time to wait for redraw
vim.opt.maxmempattern = 2000 -- Maximum memory for pattern matching

-- Format options - remove auto-commenting
vim.opt.formatoptions:remove { 'c', 'r', 'o' }

-- Make keyword include hyphen (useful for CSS, etc.)
vim.opt.iskeyword:append '-'

-- Improve diff algorithm
vim.opt.diffopt:append 'linematch:60'

-- Reduce message verbosity
vim.opt.shortmess:append 'c'
