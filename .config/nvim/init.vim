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

filetype on

filetype plugin on

" Turn off modelines
set modelines=0

" Automatically wrap text that extends beyond the screen length.
set wrap

set title
set titlestring=Neovim
set guifont=fira\ code:h11
" Uncomment below to set the max textwidth. Use a value corresponding to the width of your screen.
set textwidth=79
set formatoptions=tcqrn1
set tabstop=2
set shiftwidth=2
set softtabstop=2
set noexpandtab
set noshiftround
set smartindent

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

set foldmethod=expr
set indentexpr=vim_treesitter#indent()

" ---------------PLUGINS--------------------
"
"Plugin autoinstalling
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

autocmd VimEnter * TwilightEnable

"Syntax highlighting and autocompletion
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/cmp-omni'
Plug 'ThePrimeagen/refactoring.nvim'
Plug 'rcarriga/nvim-notify'
Plug 'michaelb/sniprun', {'do': 'bash install.sh'}
Plug 'rafamadriz/friendly-snippets'
Plug 'abecodes/tabout.nvim'
Plug 'lukas-reineke/lsp-format.nvim'
Plug 'https://git.sr.ht/~whynothugo/lsp_lines.nvim'
Plug 'ray-x/lsp_signature.nvim'
Plug 'jose-elias-alvarez/null-ls.nvim'
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
Plug 'williamboman/nvim-lsp-installer'
Plug 'L3MON4D3/LuaSnip'
Plug 'gcmt/wildfire.vim'
Plug 'alvan/vim-closetag'
Plug 'tpope/vim-eunuch'
Plug 'mg979/vim-visual-multi', {'branch': 'master'}
Plug 'mattn/emmet-vim'
Plug 'AndrewRadev/tagalong.vim'
Plug 'tpope/vim-surround'
Plug 'kana/vim-textobj-user' | Plug 'whatyouhide/vim-textobj-xmlattr'
Plug 'editorconfig/editorconfig-vim'
Plug 'mbbill/undotree'
"Syntax Highlight
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'lewis6991/nvim-treesitter-context'
Plug 'm-demare/hlargs.nvim'
Plug 'hasufell/ghcup.vim'
Plug 'rbgrouleff/bclose.vim'
Plug 'nathom/filetype.nvim'
Plug 'junegunn/vim-easy-align'
Plug 'gyim/vim-boxdraw' 
Plug 'haringsrob/nvim_context_vt'
Plug 'windwp/nvim-autopairs'
Plug 'windwp/nvim-ts-autotag'
Plug 'p00f/nvim-ts-rainbow'
Plug 'JoosepAlviste/nvim-ts-context-commentstring'
Plug 'nvim-lua/plenary.nvim'

"File search and navigation
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'christoomey/vim-tmux-navigator'
Plug 'matze/vim-move'
Plug 'ahmedkhalf/project.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'

"Editor interface and theming
Plug 'sainnhe/gruvbox-material'
Plug 'KabbAmine/yowish.vim'
Plug 'jacoborus/tender.vim'
Plug 'arcticicestudio/nord-vim'
Plug 'cocopon/iceberg.vim'
Plug 'mhinz/vim-startify'
Plug 'lambdalisue/suda.vim'
Plug 'norcalli/nvim-colorizer.lua'
Plug 'folke/twilight.nvim'
Plug 'kyazdani42/nvim-web-devicons'
Plug 'kyazdani42/nvim-tree.lua'
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
let g:neovide_cursor_vfx_mode = "torpedo"
let g:neovide_cursor_vfx_particle_density=20.0
let g:neovide_cursor_vfx_particle_speed=20.0
let g:neovide_cursor_vfx_opacity=200.0
let g:neovide_refresh_rate=60

autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb()
autocmd BufEnter * ++nested if winnr('$') == 1 && bufname() == 'NvimTree_' . tabpagenr() | quit | endif

colorscheme yowish

autocmd! BufEnter *.hs call timer_start(50, { tid -> execute('colorscheme gruvbox-material')})
autocmd! BufEnter *.js call timer_start(50, { tid -> execute('colorscheme yowish')})
autocmd! BufEnter *.c,*.ts,*.tsx,*.lua call timer_start(50, { tid -> execute('colorscheme iceberg')})
autocmd! BufEnter *.cpp,*.gd,*.tsx call timer_start(50, { tid -> execute('colorscheme nord')})
autocmd! BufEnter *.purs,*.cs call timer_start(50, { tid -> execute('colorscheme tender')})

nnoremap <silent> <C-n> :NvimTreeToggle<CR>

let g:VM_maps = {}
let g:VM_maps['Find Under'] = '<C-g>'           " replace visual C-n
let g:VM_maps['Find Subword Under'] = '<C-g>'           " replace visual C-n

vmap <silent> <C-v> <Esc>"+gp
vmap <silent> <C-c> "+y
"Misc
:imap II <Esc>
nnoremap <TAB> w
nnoremap <S-TAB> b
nnoremap <C-a> :CodeActionMenu<CR>
nnoremap <silent> <C-p> ggVG:SnipRun<CR><C-o>
vnoremap <silent> <C-p> :SnipRun<CR>

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

nnoremap <silent> <C-q> :BufferClose <CR>
:nnoremap <silent> J :BufferNext <CR>
:nnoremap <silent> K :BufferPrevious <CR>
nnoremap <silent> <C-t> :Twilight<CR>

vnoremap <C-n> :norm

"Search shortcuts
let mapleader = ","
noremap <leader>fs :Files<cr>
nnoremap <leader>fp <cmd>Telescope projects<cr>


autocmd TextChangedI,TextChangedP * call s:on_complete_check()
function! s:on_complete_check() abort
lua <<EOF
  local before_line = string.sub(vim.api.nvim_get_current_line(), 1, vim.api.nvim_win_get_cursor(0)[2] + 1)
  if string.match(before_line, '%s$') then
    require('cmp').complete()
  end
EOF
endfunction

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

require'nvim-treesitter.configs'.setup {
  ensure_installed = { "c", "lua", "rust", "haskell", "javascript", "css", "typescript" },
  sync_install = true,
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
  rainbow = {
      enable = true,
      extended_mode = true
  },
  context_commentstring = {
      enable = true
  }, 
  indent = {
		enable = true,
	},
}

require("zen-mode").setup {}

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

require'treesitter-context'.setup{
    enable = true, -- Enable this plugin (Can be enabled/disabled later via commands)
    max_lines = 1, -- How many lines the window should span. Values <= 0 mean no limit.
    patterns = { -- Match patterns for TS nodes. These get wrapped to match at word boundaries.
        -- For all filetypes
        -- Note that setting an entry here replaces all other patterns for this entry.
        -- By setting the 'default' entry below, you can control which nodes you want to
        -- appear in the context window.
        default = {
            'class',
            'function',
            'method',
            -- 'for', -- These won't appear in the context
            -- 'while',
            -- 'if',
            -- 'switch',
            -- 'case',
        },
        -- Example for a specific filetype.
        -- If a pattern is missing, *open a PR* so everyone can benefit.
        --   rust = {
        --       'impl_item',
        --   },
    },
    exact_patterns = {
        -- Example for a specific filetype with Lua patterns
        -- Treat patterns.rust as a Lua pattern (i.e "^impl_item$" will
        -- exactly match "impl_item" only)
        -- rust = true,
    },

    -- [!] The options below are exposed but shouldn't require your attention,
    --     you can safely ignore them.

    zindex = 20, -- The Z-index of the context window
}

local ft_to_parser = require"nvim-treesitter.parsers".filetype_to_parsername
ft_to_parser.purescript = "haskell"
ft_to_parser.gd = "gdscript"
require("lsp_config")

require("luasnip.loaders.from_vscode").lazy_load()
EOF
let g:cursorhold_updatetime = 100

nmap <leftmouse> <plug>(ScrollViewLeftMouse)
vmap <leftmouse> <plug>(ScrollViewLeftMouse)
imap <leftmouse> <plug>(ScrollViewLeftMouse)

" For changing choices in choiceNodes (not strictly necessary for a basic setup).
imap <silent><expr> <C-E> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-E>'
smap <silent><expr> <C-E> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-E>'
