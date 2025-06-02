require "nvchad.options"

local o = vim.o
local opt = vim.opt

o.cursorlineopt ='both'
o.relativenumber = true
o.guicursor = 'n-v-c-sm-i-ci-ve:block,r-cr-o:hor20,a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor'
opt.clipboard = "unnamedplus" -- This needs to Install xclip

