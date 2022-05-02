"   ██    ██ ██ ███    ███ ██████   ██████
"   ██    ██ ██ ████  ████ ██   ██ ██
"   ██    ██ ██ ██ ████ ██ ██████  ██
"    ██  ██  ██ ██  ██  ██ ██   ██ ██
"██   ████   ██ ██      ██ ██   ██  ██████

"---------GENERAL SETTINGS------------


"Set compatibility to Vim only.
set nocompatible
set nolist
set rnu
set termguicolors

syntax on
 
filetype on
"Helps force plug-ins to load correctly when it is turned back on below.
filetype plugin indent on

" Turn off modelines
set modelines=0

" Automatically wrap text that extends beyond the screen length.
set wrap

" Uncomment below to set the max textwidth. Use a value corresponding to the width of your screen.
set textwidth=79
set formatoptions=tcqrn1
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set noshiftround

" Display 5 lines above/below the cursor when scrolling with a mouse.
set scrolloff=5

" Fixes common backspace problems
set backspace=indent,eol,start

" Speed up scrolling in Vim
set ttyfast

" Status bar
set laststatus=2

" Display options
set showmode
set showcmd

" Highlight matching pairs of brackets. Use the '%' character to jump between them.
set matchpairs+=<:>

" Show line numbers
set number

set updatetime=500
" Encoding
set encoding=utf-8

" Highlight matching search patterns
set hlsearch

" Enable incremental search
set incsearch

" Include matching uppercase words with lowercase search term
set ignorecase

" Include only uppercase words with uppercase search term
set smartcase

" Store info from no more than 100 files at a time, 9999 lines of text, 100kb of data. Useful for copying large amounts of data between files.
set viminfo='100,<9999,s100


" ---------------PLUGINS--------------------
"
"Plugin autoinstalling
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')


"Syntax highlighting and autocompletion
Plug 'neovim/nvim-lspconfig'
Plug 'https://git.sr.ht/~whynothugo/lsp_lines.nvim'
Plug 'ray-x/lsp_signature.nvim'
Plug 'onsails/lspkind.nvim'
Plug 'weilbith/nvim-code-action-menu'
Plug 'kosayoda/nvim-lightbulb'
Plug 'p00f/clangd_extensions.nvim'
Plug 'tzachar/cmp-tabnine', { 'do': './install.sh' }
Plug 'rafamadriz/friendly-snippets'
Plug 'saadparwaiz1/cmp_luasnip'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-cmdline'
Plug 'hrsh7th/nvim-cmp'
Plug 'nvim-lua/completion-nvim'
Plug 'nvim-lua/diagnostic-nvim'
Plug 'williamboman/nvim-lsp-installer'
Plug 'L3MON4D3/LuaSnip'
Plug 'gcmt/wildfire.vim'
Plug 'alvan/vim-closetag'
Plug 'LunarWatcher/auto-pairs'
Plug 'tpope/vim-eunuch'
Plug 'mg979/vim-visual-multi', {'branch': 'master'}
Plug 'dense-analysis/ale'
Plug 'mattn/emmet-vim'
Plug 'AndrewRadev/tagalong.vim'
Plug 'tpope/vim-surround'
Plug 'kana/vim-textobj-user' | Plug 'whatyouhide/vim-textobj-xmlattr'
Plug 'editorconfig/editorconfig-vim'
Plug 'mbbill/undotree'
"Syntax Highlight
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'm-demare/hlargs.nvim'
Plug 'hasufell/ghcup.vim'
Plug 'rbgrouleff/bclose.vim'
Plug 'nathom/filetype.nvim'
Plug 'junegunn/vim-easy-align'
Plug 'gyim/vim-boxdraw' 
Plug 'haringsrob/nvim_context_vt'

"File search and navigation
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'preservim/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'christoomey/vim-tmux-navigator'
Plug 'matze/vim-move'

"Editor interface and theming
Plug 'morhetz/gruvbox'
Plug 'jacoborus/tender.vim'
Plug 'arcticicestudio/nord-vim'
Plug 'cocopon/iceberg.vim'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
Plug 'mhinz/vim-startify'
Plug 'lambdalisue/suda.vim'
Plug 'norcalli/nvim-colorizer.lua'
Plug 'folke/twilight.nvim'
Plug 'kyazdani42/nvim-web-devicons'
Plug 'romgrk/barbar.nvim'
Plug 'lukas-reineke/indent-blankline.nvim'
Plug 'folke/zen-mode.nvim'
Plug 'dstein64/nvim-scrollview'
Plug 'edluffy/specs.nvim'
Plug 'nvim-lualine/lualine.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'

"Debugging, refactoring and version control
Plug 'puremourning/vimspector'
Plug 'antoinemadec/FixCursorHold.nvim'

call plug#end()

set completeopt=menu,menuone,noselect
"---------- PLUGIN VARIABLES---------------
"
let g:suda_smart_edit = 1
let g:airline_powerline_fonts = 1
let g:NERDTreeWinPos = "right"
autocmd VimEnter * NERDTree 
autocmd VimEnter * NERDTreeToggle
" Exit Vim if NERDTree is the only window remaining in the only tab.
autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
autocmd BufEnter * if bufname('#') =~ 'NERD_tree_\d\+' && bufname('%') !~ 'NERD_tree_\d\+' && winnr('$') > 1 |
    \ let buf=bufnr() | buffer# | execute "normal! \<C-W>w" | execute 'buffer'.buf | endif

autocmd vimenter * ++nested colorscheme gruvbox
autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb()
autocmd BufEnter,FileType *
\   if &ft ==# 'c' || &ft ==# 'cpp' | colorscheme iceberg |
\   elseif &ft ==? 'haskell' | colorscheme gruvbox |
\   elseif &ft ==? 'purescript' | colorscheme tender |
\   elseif &ft ==? 'nerdtree' | colorscheme |
\   else | colorscheme iceberg |
\   endif

nmap <C-n> :NERDTreeToggle <CR>
vmap <C-v> <Esc>"+gp
"Misc
:imap II <Esc>
:map H b
:map L w
noremap <TAB> w
map <S-TAB> b
nmap <C-a> \\

let g:AutoPairsCompatibleMaps = 0 
let g:AutoPairsMapBS = 1
let g:AutoPairsShortcutToggle = 0
let g:AutoPairsShortcutJump = "<C-e>"
let g:AutoPairsMultilineClose = 1
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

map <SPACE> <Plug>(wildfire-fuel)
let g:wildfire_objects = {
    \ "*" : ["i'", 'i"', "i)", "i]", "i}", "i`"]
\ }

cal wildfire#triggers#Add("<ENTER>", {
    \ "html,xml" : ["at", "it"],
\ })

nmap <silent> <C-t> :tab sball <CR>
nmap <C-q> :q! <CR>
:nmap <silent> J :tabn <CR>
:nmap <silent> K :tabp <CR>

vnoremap <C-n> :norm

"Search shortcuts
let mapleader = ","
noremap <leader>w :w<cr>
noremap <leader>gs :CocSearch
noremap <leader>fs :Files<cr>
noremap <leader><cr> <cr><c-w>h:q<cr>

" Vim's auto indentation feature does not work properly with text copied from outside of Vim. Press the <F2> key to toggle paste mode on/off.
nnoremap <F2> :set invpaste paste?<CR>
imap <F2> <C-O>:set invpaste paste?<CR>
set pastetoggle=<F2>

command! -nargs=0 Prettier :CocCommand prettier.formatFile

let g:NERDTreeIgnore = ['^node_modules$']
let g:VM_maps = {}
let g:VM_maps['Find Under']         = '<C-d>'           " replace C-n
let g:VM_maps['Find Subword Under'] = '<C-d>'           " replace visual C-n
let g:VM_maps["Select Cursor Down"] = '<C-j>'      " start selecting down
let g:VM_maps["Select Cursor Up"]   = '<C-k>'        " start selecting up

let NERDTreeShowHidden = 1
let NERDTreeCustomOpenArgs = {'file': {'reuse':'all', 'where':'t', 'keepopen':0, 'stay':0}}
let NERDTreeMapCustomOpen = 't'

let g:user_emmet_leader_key=','
let g:user_emmet_settings = {
\  'variables': {'lang': 'ja'},
\  'html': {
\    'default_attributes': {
\      'option': {'value': v:null},
\      'textarea': {'id': v:null, 'name': v:null, 'cols': 10, 'rows': 10},
\    },
\    'snippets': {
\      'html:5': "<!DOCTYPE html>\n"
\              ."<html lang=\"${lang}\">\n"
\              ."<head>\n"
\              ."\t<meta charset=\"${charset}\">\n"
\              ."\t<title></title>\n"
\              ."\t<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
\              ."</head>\n"
\              ."<body>\n\t${child}|\n</body>\n"
\              ."</html>",
\    },
\  },
\}
let g:airline_theme='jellybeans'

lua << EOF
require'lspconfig'.hls.setup{}

require'nvim-treesitter.configs'.setup {
  ensure_installed = { "c", "lua", "rust", "haskell", "javascript", "css", "typescript" },
  sync_install = true,
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = true,
  },
}


require("zen-mode").setup {
  -- your configuration comes here
  -- or leave it empty to use the default settings
  -- refer to the configuration section below
}
require("filetype").setup({
    overrides = {
        extensions = {
            -- Set the filetype of *.pn files to potion
            purs = "purescript",
        },
        complex = {
            -- Set the filetype of any full filename matching the regex to gitconfig
            [".*git/config"] = "gitconfig", -- Included in the plugin
        },

        -- The same as the ones above except the keys map to functions
        function_extensions = {
            ["cpp"] = function()
                vim.bo.filetype = "cpp"
                -- Remove annoying indent jumping
                vim.bo.cinoptions = vim.bo.cinoptions .. "L0"
            end,
            ["pdf"] = function()
                vim.bo.filetype = "pdf"
                -- Open in PDF viewer (Skim.app) automatically
                vim.fn.jobstart(
                    "open -a skim " .. '"' .. vim.fn.expand("%") .. '"'
                )
            end,
        },
        function_literal = {
            Brewfile = function()
                vim.cmd("syntax off")
            end,
        },
        function_complex = {
            ["*.math_notes/%w+"] = function()
                vim.cmd("iabbrev $ $$")
            end,
        },

        shebang = {
            -- Set the filetype of files with a dash shebang to sh
            dash = "sh",
        },
    },
})
local ft_to_parser = require"nvim-treesitter.parsers".filetype_to_parsername
ft_to_parser.purescript = "haskell"
require("lsp_config")
EOF
let g:ale_disable_lsp = 1
let g:ale_linters = {'haskell': ['hlint'], 'css': ['fecs'], 'javascript': ['eslint', 'jscs', 'prettier', 'tsserver', 'flow']}
let g:ale_fixers = {'haskell': ['ormolu', 'floskell' ], 'purescript': ['purs-tidy']}
let g:cursorhold_updatetime = 100

nmap <leftmouse> <plug>(ScrollViewLeftMouse)
vmap <leftmouse> <plug>(ScrollViewLeftMouse)
imap <leftmouse> <plug>(ScrollViewLeftMouse)

