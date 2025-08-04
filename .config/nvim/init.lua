vim.o.number = true
vim.o.relativenumber = true
vim.o.cursorline = true
vim.o.scrolloff = 10
vim.o.sidescrolloff = 5
vim.o.wrap = false

vim.o.tabstop = 2
vim.o.shiftwidth = 2
vim.o.softtabstop = 2
vim.o.expandtab = true
vim.o.autoindent = true

vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.hlsearch = false
vim.o.incsearch = true

vim.o.termguicolors = true
vim.o.showmatch = false
vim.o.showmode = false
vim.o.completeopt = "menuone,noinsert,noselect"
vim.o.winblend = 0

vim.o.backup = false
vim.o.writebackup = false
vim.o.swapfile = false
vim.o.undofile = true
vim.o.updatetime = 300
vim.o.autoread = true
vim.o.autowrite = false

vim.o.hidden = true
vim.o.backspace = "indent,eol,start"
vim.o.autochdir = false

vim.keymap.set("n", "<A-j>", ":m .+1<CR>==", { silent = true })
vim.keymap.set("n", "<A-k>", ":m .-2<CR>==", { silent = true })
vim.keymap.set("v", "<A-j>", ":m '>+1<CR>gv=gv", { silent = true })
vim.keymap.set("v", "<A-k>", ":m '<-2<CR>gv=gv", { silent = true })


vim.keymap.set("v", "<", "<gv", { silent = true })
vim.keymap.set("v", ">", ">gv", { silent = true })

vim.o.wildmenu = true
vim.o.wildmode = "longest:full,full"

vim.g.mapleader = ","

vim.pack.add({
  { src = "https://github.com/neovim/nvim-lspconfig"},
  { src = "https://github.com/stevearc/oil.nvim"},
  { src = "https://github.com/nvim-treesitter/nvim-treesitter", version = 'main' },
  "https://github.com/nvim-treesitter/nvim-treesitter-context",
  "https://github.com/echasnovski/mini.ai",
})

