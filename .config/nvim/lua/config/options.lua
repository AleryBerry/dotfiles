local opt               = vim.opt
local g                 = vim.g

g.suda_smart_edit       = 1
g.toggle_theme_icon     = "   "
g.theme_switcher_loaded = false
opt.autochdir           = true

opt.foldcolumn          = 'auto:1'
opt.fillchars           = 'eob: ,fold: ,foldopen:,foldsep:|,foldclose:'
opt.foldenable          = true
opt.foldlevel           = 99 -- Using ufo provider need a large value, feel free to decrease the value
opt.foldlevelstart      = 99
opt.foldmethod          = "expr"
opt.foldexpr            = "v:lua.vim.treesitter.foldexpr()"
opt.foldtext            = "v:lua.vim.treesitter.foldtext()"

vim.filetype.on         = true

opt.hlsearch            = false
opt.scrolloff           = 10

g.loaded_netrw          = 1
g.loaded_netrwPlugin    = 1

opt.laststatus          = 3 -- global statusline
opt.showmode            = false

opt.clipboard           = "unnamedplus"
opt.cursorline          = true

-- Indenting
vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.autoindent = true
vim.opt.smartindent = false
vim.g.rust_recommended_style = false

opt.ignorecase          = true
opt.smartcase           = true
opt.mouse               = "a"

-- Numbers
opt.relativenumber      = true
opt.number              = true
opt.ignorecase          = true

-- disable nvim intro
opt.shortmess:append("sI")

opt.signcolumn = "yes"
opt.splitbelow = true
opt.splitright = true
opt.termguicolors = true
opt.timeoutlen = 400
opt.undofile = true

-- interval for writing swap file to disk, also used by gitsigns
opt.updatetime = 250

vim.keymap.set('n',        's', '<Plug>(leap-anywhere)')
vim.keymap.set({'x', 'o'}, 's', '<Plug>(leap)')
vim.diagnostic.config({ virtual_text = false })

vim.g.mapleader = ","
vim.g.maplocalleader = ","
