local opt = vim.opt
local g = vim.g

g.suda_smart_edit = 1
g.toggle_theme_icon = "   "
g.theme_switcher_loaded = false

vim.filetype.on = true

g.loaded_netrw = 1
g.loaded_netrwPlugin = 1

opt.laststatus = 3 -- global statusline
opt.showmode = false

opt.clipboard = "unnamedplus"
opt.cursorline = true

-- Indenting
opt.expandtab = true
opt.shiftwidth = 2
opt.smartindent = true
opt.tabstop = 2
opt.softtabstop = 2

opt.fillchars = { eob = " " }
opt.ignorecase = true
opt.smartcase = true
opt.mouse = "a"

-- Numbers
opt.relativenumber = true
opt.number = true
opt.ignorecase = true

-- disable nvim intro
opt.shortmess:append "sI"

opt.signcolumn = "yes"
opt.splitbelow = true
opt.splitright = true
opt.termguicolors = true
opt.timeoutlen = 400
opt.undofile = true

-- interval for writing swap file to disk, also used by gitsigns
opt.updatetime = 250

-- go to previous/next line with h,l,left arrow and right arrow
-- when cursor reaches end/beginning of line
opt.whichwrap:append "<>[]hl"
opt.wrap = true


function map(mode, lhs, rhs, opts)
  local options = { noremap = true }
  if opts then
    options = vim.tbl_extend("force", options, opts)
  end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

map("n", "<C-n>", ":NvimTreeToggle<CR>", { silent = true })
map("n", "<C-a>", ":<cmd>AerialToggle!<CR>", { silent = true })
map("i", "JJ", "<Esc>", { silent = true })
map("v", "<C-v>", "<Esc>\"+gp", { silent = true })
map("v", "<C-c>", "\"+y", { silent = true })

map("n", "<C-q>", ":BufferClose<CR>", { silent = true })

map('n', '<A-w>', '<Cmd>BufferPrevious<CR>', opts)
map('n', '<A-q>', '<Cmd>BufferNext<CR>', opts)

map("v", "<SPACE>", "<Plug>(expand_region_expand)", { silent = true })
map("n", "<SPACE>", "<Plug>(expand_region_expand)", { silent = true })
map("v", "<C-SPACE>", "<Plug>(expand_region_shrink)", { silent = true })
map("n", "<C-SPACE>", "<Plug>(expand_region_shrink)", { silent = true })

map('n', '<A-j>', ':MoveLine(1)<CR>', { silent = true })
map('n', '<A-k>', ':MoveLine(-1)<CR>', { silent = true })
map('n', '<A-h>', ':MoveHChar(-1)<CR>', { silent = true })
map('n', '<A-l>', ':MoveHChar(1)<CR>', { silent = true })

map('v', '<A-j>', ':MoveBlock(1)<CR>', { silent = true })
map('v', '<A-k>', ':MoveBlock(-1)<CR>', { silent = true })
map('v', '<A-h>', ':MoveHBlock(-1)<CR>', { silent = true })
map('v', '<A-l>', ':MoveHBlock(1)<CR>', { silent = true })

map("n", "<C-p>", "ggVG:SnipRun<CR><C-o>", { silent = true })
map("v", "<C-p>", ":SnipRun<CR>", { silent = true })

map("n", ",p", ":Telescope projects<CR>", { silent = true })

vim.cmd "autocmd BufWritePre * lua vim.lsp.buf.format({ async = false })"
vim.cmd "autocmd BufEnter *.hs call timer_start(50, { tid -> execute('colorscheme gruvbox-material')})"
vim.cmd "autocmd BufEnter *.js,*.dart call timer_start(50, { tid -> execute('colorscheme yowish')})"
vim.cmd "autocmd BufEnter *.c,*.ts,*.tsx,*.lua call timer_start(50, { tid -> execute('colorscheme iceberg')})"
vim.cmd "autocmd BufEnter *.cpp,*.gd,*.tsx call timer_start(50, { tid -> execute('colorscheme dracula')})"
vim.cmd "autocmd BufEnter *.purs,*.cs call timer_start(50, { tid -> execute('colorscheme tender')})"

if g.neovide then
  g.neovide_fullscreen = true
  g.neovide_transparency = 0.95
  opt.guifont = "Iosevka Nerd Font,Noto Color Emoji:h14"
end

vim.diagnostic.config({
  virtual_lines = true,
  virtual_text = false,
  update_in_insert = false,
})

vim.g.mapleader = ","

map("i", "<leader>,", "<Plug>(emmet-expand-abbr)", { silent = true })
vim.g.emmet_install_only_plug = 1

g.VM_default_mappings = 0
g.VM_maps = {
  ['Find Under'] = '<C-g>',
  ['Find Subword Under'] = '<C-g>',
  ['Add Cursor Up'] = '<C-k>',
  ['Add Cursor Down'] = '<C-j>',
}
vim.api.nvim_create_autocmd("InsertEnter", {
  pattern = "*",
  callback = function()
    vim.diagnostic.enable(false, {})
  end
})
vim.api.nvim_create_autocmd("InsertLeave", {
  pattern = "*",
  callback = function()
    vim.diagnostic.enable(true, {})
  end
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "gdscript",
  callback = function()
    vim.opt.expandtab = false
    vim.opt.smartindent = true
    vim.opt.tabstop = 2
    vim.opt.softtabstop = 2
  end
})
