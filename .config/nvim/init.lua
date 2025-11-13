vim.o.number = true
vim.o.relativenumber = true
vim.o.cursorline = true
vim.o.scrolloff = 10
vim.o.sidescrolloff = 5
vim.o.wrap = true
vim.o.linebreak = true

vim.o.tabstop = 2
vim.o.shiftwidth = 2
vim.o.softtabstop = 2
vim.o.expandtab = true
vim.o.autoindent = false
vim.o.smartindent = false

vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.hlsearch = false
vim.o.incsearch = true

vim.o.termguicolors = true
vim.o.showmatch = false
vim.o.showmode = false
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


vim.keymap.set("n", "j", "gj", { silent = true })
vim.keymap.set("n", "k", "gk", { silent = true })

vim.keymap.set("v", "<", "<gv", { silent = true })
vim.keymap.set("v", ">", ">gv", { silent = true })

-- vim.keymap.set('t', '<Esc>', "<C-\\><C-n>", { silent = true })

vim.keymap.set('n', '<c-q>', "<cmd>bdelete!<CR>", { silent = true })

vim.o.wildmenu = true
vim.o.wildmode = "longest:full,full"

vim.g.mapleader = vim.keycode("<F20>")

vim.pack.add({
	"https://github.com/echasnovski/mini.ai",
	"https://github.com/kwkarlwang/bufjump.nvim",
	"https://github.com/cbochs/grapple.nvim",
	"https://github.com/nvim-tree/nvim-web-devicons",
	"https://github.com/sainnhe/gruvbox-material",
	"https://github.com/shellRaining/hlchunk.nvim",
	"https://github.com/ggandor/leap.nvim",
	"https://github.com/gbprod/substitute.nvim",
	"https://github.com/gbprod/yanky.nvim",
	"https://github.com/oribarilan/lensline.nvim",
	-- lspconfig
	"https://github.com/neovim/nvim-lspconfig",
	"https://github.com/nvim-lua/plenary.nvim",
	"https://github.com/pmizio/typescript-tools.nvim",
	"https://github.com/aznhe21/actions-preview.nvim",
	"https://github.com/hasansujon786/nvim-navbuddy",
	"https://github.com/neovim/nvim-lspconfig",
	"https://github.com/MunifTanjim/nui.nvim",
	"https://github.com/mfussenegger/nvim-jdtls",
	"https://github.com/nvim-flutter/flutter-tools.nvim",
	"https://github.com/mrcjkb/haskell-tools.nvim",
	-- lualine
	"https://github.com/nvim-lualine/lualine.nvim",
	"https://github.com/SmiteshP/nvim-navic",
	-- multicursors
	"https://github.com/smoka7/multicursors.nvim",
	"https://github.com/nvimtools/hydra.nvim",

	"https://github.com/kylechui/nvim-surround",

	"https://github.com/Fildo7525/pretty_hover",

	"https://github.com/rachartier/tiny-inline-diagnostic.nvim",
	"https://github.com/MeanderingProgrammer/render-markdown.nvim",

	-- Treesitter
	{ src = "https://github.com/nvim-treesitter/nvim-treesitter",             version = "main" },
	{ src = "https://github.com/nvim-treesitter/nvim-treesitter-textobjects", version = "main" },

 "https://github.com/folke/snacks.nvim" ,
})

vim.g.neovide_opacity = 0.9

vim.api.nvim_create_autocmd("FileType", {
	pattern = "*",
	callback = function()
		-- vim.o.tabstop = 2
		-- vim.o.shiftwidth = 2
		-- vim.o.softtabstop = 2
		-- vim.o.expandtab = false
		vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
		vim.wo.foldexpr = 'v:lua.vim.treesitter.foldexpr()'
	end,
})

require('vim._extui').enable({})
vim.o.cmdheight = 1

vim.api.nvim_create_autocmd("TermOpen", {
	group = vim.api.nvim_create_augroup("custom-term-open", { clear = true }),
	callback = function()
		vim.opt.number = false
		vim.opt.relativenumber = false
	end
})

vim.keymap.set("n", "<leader>t", "Floaterminal", { silent = true})
