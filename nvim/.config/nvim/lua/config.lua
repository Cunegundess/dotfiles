vim.g.termguicolors = true
vim.g.have_nerd_font = true

vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.signcolumn = "yes"
vim.opt.colorcolumn = ""
vim.opt.cursorline = true
vim.opt.cursorcolumn = true
vim.opt.wrap = false
vim.opt.linebreak = true
vim.opt.showcmd = true
vim.opt.cmdheight = 1
vim.opt.pumheight = 10
vim.opt.pumblend = 10

vim.o.shiftwidth = 2
vim.o.tabstop = 2
vim.o.softtabstop = 2
vim.o.expandtab = false
vim.o.autoindent = true
vim.o.smartindent = true
vim.opt.shiftround = true
vim.opt.copyindent = true

vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.inccommand = "split"

vim.opt.mouse = "a"

vim.schedule(function()
	vim.opt.clipboard = "unnamedplus"
end)

vim.opt.undofile = true
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.autoread = true

vim.opt.scrolloff = 10
vim.opt.sidescrolloff = 8
vim.opt.smoothscroll = true

vim.opt.updatetime = 250
vim.opt.timeoutlen = 300
vim.opt.ttimeoutlen = 10

vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.equalalways = true

vim.opt.list = true
vim.opt.listchars = {
	tab = "· ",
	trail = "·",
	nbsp = "␣",
	extends = "❯",
	precedes = "❮",
}

vim.opt.completeopt = { "menu", "menuone", "noselect" }
vim.opt.wildmenu = true
vim.opt.wildmode = "longest:full,full"
vim.opt.wildoptions = "pum"

vim.opt.breakindent = true
vim.opt.confirm = true
vim.opt.hidden = true
vim.opt.selection = "exclusive"
vim.opt.virtualedit = "block"

vim.opt.redrawtime = 1500
vim.opt.maxmempattern = 2000

vim.opt.formatoptions:remove({ "c", "r", "o" })

vim.opt.iskeyword:append("-")

vim.opt.diffopt:append("linematch:60")

vim.opt.shortmess:append("c")

vim.opt.laststatus = 3

local branch_cache = ""

function _G.git_branch()
	if branch_cache ~= "" then
		return branch_cache
	end

	local handle = io.popen("git branch --show-current 2>/dev/null")
	if handle then
		local result = handle:read("*a")
		handle:close()
		result = result:gsub("%s+", "")
		if result ~= "" then
			branch_cache = "  " .. result .. " "
			return branch_cache
		end
	end

	return ""
end

vim.o.statusline = table.concat({
	"%f",
	" %m",
	" %r",
	"%=",
	"%{v:lua.git_branch()}",
	" %y",
	" %p%%",
	" %l:%c",
})
