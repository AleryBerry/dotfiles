-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out,                            "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

-- Make sure to setup `mapleader` and `maplocalleader` before
-- loading lazy.nvim so that mappings are correct.
-- This is also a good place to setup other settings (vim.opt)
require('settings')

-- Setup lazy.nvim
require("lazy").setup({
  spec = {
    'wbthomason/packer.nvim',

    {
      'windwp/nvim-autopairs',
      opts = {}
    },
    -- Lazy loading:
    -- Load on specific commands
    { 'tpope/vim-dispatch',   lazy = true,       cmd = { 'Dispatch', 'Make', 'Focus', 'Start' } },

    -- Load on an autocommand event
    { 'andymass/vim-matchup', event = 'VimEnter' },
    -- Post-install/update hook with neovim command
    {
      "nvim-treesitter/nvim-treesitter",
      build = ":TSUpdate",
      config = function()
        local configs = require("nvim-treesitter.configs")

        configs.setup({
          ensure_installed = { "c", "gdscript", "lua", "zig", "vim", "vimdoc", "query", "elixir", "heex", "javascript", "html" },
          sync_install = false,
          highlight = { enable = true },
          indent = { enable = true },
        })
      end
    },

    --  specific branch, dependency and run lua file after load
    {
      'nvim-lualine/lualine.nvim',
      dependencies = { 'nvim-tree/nvim-web-devicons' }
    },
    --  dependency and run lua function after load
    {
      'lewis6991/gitsigns.nvim',
      dependencies = { 'nvim-lua/plenary.nvim' },
      opts = {},
    },
    {
      'dgagn/diagflow.nvim',
      event = 'LspAttach',
      opts = {
        placement = 'top',
        inline_padding_left = 1,
        scope = 'line',
        max_height = 600,
      },
    },

    {
      "nvim-tree/nvim-tree.lua",
      version = "*",
      lazy = false,
      dependencies = {
        "nvim-tree/nvim-web-devicons",
      },
      config = function()
        require("nvim-tree").setup({


          auto_reload_on_write = true,
          disable_netrw = false,
          hijack_cursor = false,
          hijack_netrw = true,
          hijack_unnamed_buffer_when_opening = false,
          open_on_tab = false,
          sort_by = "name",
          update_cwd = true,
          sync_root_with_cwd = true,
          respect_buf_cwd = true,
          view = {
            width = 30,
            side = "right",
            preserve_window_proportions = false,
            number = false,
            relativenumber = false,
            signcolumn = "yes",
          },
          renderer = {
            indent_markers = {
              enable = true,
              icons = {
                corner = "└ ",
                edge = "│ ",
                none = "  ",
              },
            },
            icons = {
              webdev_colors = true,
              git_placement = "before",
            }
          },
          hijack_directories = {
            enable = true,
            auto_open = true,
          },
          update_focused_file = {
            enable = true,
            update_cwd = true,
            update_root = true,
            ignore_list = {},
          },
          system_open = {
            cmd = "",
            args = {},
          },
          diagnostics = {
            enable = false,
            show_on_dirs = false,
            icons = {
              hint = "",
              info = "",
              warning = "",
              error = "",
            },
          },
          filters = {
            dotfiles = false,
            custom = {},
            exclude = {},
          },
          git = {
            enable = true,
            ignore = true,
            timeout = 400,
          },
          actions = {
            use_system_clipboard = true,
            change_dir = {
              enable = true,
              global = true,
              restrict_above_cwd = false,
            },
            open_file = {
              quit_on_open = true,
              resize_window = false,
              window_picker = {
                enable = true,
                chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890",
                exclude = {
                  filetype = { "notify", "packer", "qf", "diff", "fugitive", "fugitiveblame" },
                  buftype = { "nofile", "terminal", "help" },
                },
              },
            },
          },
          trash = {
            cmd = "trash",
            require_confirm = true,
          },
          log = {
            enable = false,
            truncate = false,
            types = {
              all = false,
              config = false,
              copy_paste = false,
              diagnostics = false,
              git = false,
              profile = false,
            },
          },
        })
      end,
    },
    {
      'neovim/nvim-lspconfig',
      event = { "BufReadPost", "BufNewFile" },
      cmd = { "LspInfo", "LspInstall", "LspUninstall" },
    },
    { 'hrsh7th/cmp-nvim-lsp' },
    { 'hrsh7th/cmp-buffer' },
    { 'hrsh7th/cmp-path' },
    { 'hrsh7th/cmp-cmdline' },
    {
      'hrsh7th/nvim-cmp',
      dependencies = { "saadparwaiz1/cmp_luasnip", },

      event = { "InsertEnter" },
      config = function()
        require 'cmp'.setup {
          snippet = {
            expand = function(args)
              require 'luasnip'.lsp_expand(args.body)
            end
          },

          sources = {
            { name = 'luasnip' },
            -- more sources
          },
        }
      end
    },
    {
      "L3MON4D3/LuaSnip",
      -- follow latest release.
      version = "v2.*", -- Replace <CurrentMajor> by the latest released major (first number of latest release)
      -- install jsregexp (optional!).
      build = "make install_jsregexp"
    },
    {
      "goolord/alpha-nvim",
      event = "VimEnter",
      dependencies = {
        "ozthemagician/alpha-cowsays-nvim",
      },
      config = function()
        local alpha = require("alpha")
        local startify = require("alpha.themes.startify")
        local cow = require("alpha-cowsays-nvim")

        startify.section.header.val = cow.cowsays()

        alpha.setup(startify.config)
      end,
    },
    { 'hrsh7th/cmp-nvim-lsp-signature-help' },
    {
      'akinsho/flutter-tools.nvim',
      lazy = false,
      dependencies = {
        'nvim-lua/plenary.nvim',
        'stevearc/dressing.nvim', -- optional for vim.ui.select
      },
      config = true,
    },
    { 'nvim-lua/plenary.nvim' },
    { 'akinsho/bufferline.nvim',            version = "*", dependencies = 'nvim-tree/nvim-web-devicons' },
    {
      'abecodes/tabout.nvim',
      config = function()
        require('tabout').setup {
          tabkey = '<Tab>',             -- key to trigger tabout, set to an empty string to disable
          backwards_tabkey = '<S-Tab>', -- key to trigger backwards tabout, set to an empty string to disable
          act_as_tab = true,            -- shift content if tab out is not possible
          act_as_shift_tab = false,     -- reverse shift content if tab out is not possible (if your keyboard/terminal supports <S-Tab>)
          default_tab = '<C-t>',        -- shift default action (only at the beginning of a line, otherwise <TAB> is used)
          default_shift_tab = '<C-d>',  -- reverse shift default action,
          enable_backwards = true,      -- well ...
          completion = true,            -- if the tabkey is used in a completion pum
          tabouts = {
            { open = "'", close = "'" },
            { open = '"', close = '"' },
            { open = '`', close = '`' },
            { open = '(', close = ')' },
            { open = '[', close = ']' },
            { open = '{', close = '}' },
            { open = '<', close = '>' }
          },
          ignore_beginning = true, --[[ if the cursor is at the beginning of a filled element it will rather tab out than shift the content ]]
          exclude = {} -- tabout will ignore these filetypes
        }
      end,
      dependencies = { 'nvim-treesitter', 'nvim-cmp' }, -- or require if not used so far
    },
    { 'echasnovski/mini.jump',    branch = 'stable' },
    { 'mg979/vim-visual-multi',   branch = 'master' },
    { 'lambdalisue/suda.vim' },
    { 'tpope/vim-surround' },
    { 'terryma/vim-expand-region' },
    {
      'michaelb/sniprun',
      build = 'bash ./install.sh',
      config = function() require('sniprun').setup({ display = { "NvimNotify" } }) end
    },
    { 'rcarriga/nvim-notify' },
    { 'aznhe21/actions-preview.nvim' },
    { 'mattn/emmet-vim' },
    { 'kosayoda/nvim-lightbulb' },
    {
      'm-demare/hlargs.nvim', dependencies = { 'nvim-treesitter/nvim-treesitter' } }, {
    'numToStr/Comment.nvim',
    opts = {},
  },
    { 'fedepujol/move.nvim' },
    {
      "ahmedkhalf/project.nvim",
      config = function()
        require("project_nvim").setup {
        }
      end
    },
    { 'nvim-telescope/telescope.nvim' },
    { "lukas-reineke/indent-blankline.nvim" },
    { 'dstein64/nvim-scrollview' },
    { 'romgrk/barbar.nvim',                 dependencies = 'nvim-web-devicons' },
    { 'nvim-tree/nvim-web-devicons' },

    -- Themes
    { 'KabbAmine/yowish.vim' },
    { 'cocopon/iceberg.vim' },
    { 'Mofiqul/dracula.nvim' },
    { 'jacoborus/tender.vim' },
    { 'sainnhe/gruvbox-material' },
    { 'mrcjkb/haskell-tools.nvim' },

    { 'udalov/kotlin-vim' },
    { 'mfussenegger/nvim-jdtls' },
  },
  -- Configure any other settings here. See the documentation for more details.
  -- colorscheme that will be used when installing plugins.
  install = { colorscheme = { "habamax" } },
  -- automatically check for plugin updates
  checker = { enabled = true },
}
)
require('nvim-lightbulb').setup({ autocmd = { enabled = true } })
vim.cmd("colorscheme yowish")

local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.html = {
  install_info = {
    url = "~/Git/superhtml/tree-sitter-superhtml", -- local path or git repo
    files = { "src/parser.c" },                    -- note that some parsers also require src/scanner.c or src/scanner.cc
  },
  filetype = "html",                               -- if filetype does not match the parser name
}
vim.treesitter.language.register('superhtml', 'html')

require('config.lspconfig')
require('config.treesitter')
require('config.autopairs')
require('config.telescope')
require('config.lualine')
