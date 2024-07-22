local function pconf(plugin)
  return "require(\"config." .. plugin .. "\")"
end

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
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

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
    { 'tpope/vim-dispatch',              opt = true,        cmd = { 'Dispatch', 'Make', 'Focus', 'Start' } },

    -- Load on an autocommand event
    { 'andymass/vim-matchup',            event = 'VimEnter' },
    -- Post-install/update hook with neovim command
    { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' },

    --  specific branch, dependency and run lua file after load
    {
      'nvim-lualine/lualine.nvim',
      dependencies = { 'nvim-tree/nvim-web-devicons' }
    },
    --  dependency and run lua function after load
    {
      'lewis6991/gitsigns.nvim',
      requires = { 'nvim-lua/plenary.nvim' },
      opts = {},
    },

    {
      'nvim-tree/nvim-tree.lua',
      requires = {
        'nvim-tree/nvim-web-devicons', -- optional, for file icons
      },
      { 'neovim/nvim-lspconfig' },
      { 'hrsh7th/cmp-nvim-lsp' },
      { 'hrsh7th/cmp-buffer' },
      { 'hrsh7th/cmp-path' },
      { 'hrsh7th/cmp-cmdline' },
      { 'hrsh7th/nvim-cmp' },
      {
        "L3MON4D3/LuaSnip",
        -- follow latest release.
        version = "v2.*", -- Replace <CurrentMajor> by the latest released major (first number of latest release)
        -- install jsregexp (optional!).
        build = "make install_jsregexp"
      },
      { 'hrsh7th/cmp-nvim-lsp-signature-help' },
      { 'akinsho/flutter-tools.nvim' },
      { 'nvim-lua/plenary.nvim' },
      { 'akinsho/bufferline.nvim',            tag = "v3.*", requires = 'nvim-tree/nvim-web-devicons' },
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
        wants = { 'nvim-treesitter' }, -- or require if not used so far
        after = { 'nvim-cmp' }         -- if a completion plugin is using tabs load it before
      },
      { 'echasnovski/mini.jump',    branch = 'stable' },
      { 'mg979/vim-visual-multi',   branch = 'master' },
      { 'lambdalisue/suda.vim' },
      { 'tpope/vim-surround' },
      { 'terryma/vim-expand-region' },
      {
        'michaelb/sniprun',
        run = 'bash ./install.sh',
        config = function() require('sniprun').setup({ display = { "NvimNotify" } }) end
      },
      { 'rcarriga/nvim-notify' },
      { 'mattn/emmet-vim' },
      ({ 'weilbith/nvim-code-action-menu', cmd = 'CodeActionMenu', }),
      { 'kosayoda/nvim-lightbulb' },
      {
        'm-demare/hlargs.nvim',
        requires = { 'nvim-treesitter/nvim-treesitter' }
      },
      {
        'numToStr/Comment.nvim',
        config = function()
          require('Comment').setup()
        end
      },
      { 'fedepujol/move.nvim' },
      { "ahmedkhalf/project.nvim",      config = function() require("project_nvim").setup {} end },
      { 'nvim-telescope/telescope.nvim' },
      {
        "startup-nvim/startup.nvim",
        requires = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
        config = function() require "startup".setup({ theme = "startify" }) end
      },
      { "lukas-reineke/indent-blankline.nvim" },
      { 'dstein64/nvim-scrollview' },
      { 'romgrk/barbar.nvim',                 requires = 'nvim-web-devicons' },
      { 'nvim-tree/nvim-web-devicons' },

      ({
        "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
        config = function()
          require("lsp_lines").setup()
        end,
      }),
      -- Themes
      { 'KabbAmine/yowish.vim' },
      { 'cocopon/iceberg.vim' },
      { 'Mofiqul/dracula.nvim' },
      { 'jacoborus/tender.vim' },
      { 'sainnhe/gruvbox-material' },
      { 'mrcjkb/haskell-tools.nvim' },
      { 'nathom/filetype.nvim' },

      { 'udalov/kotlin-vim' },
      { 'mfussenegger/nvim-jdtls' },
    },
    -- Configure any other settings here. See the documentation for more details.
    -- colorscheme that will be used when installing plugins.
    install = { colorscheme = { "habamax" } },
    -- automatically check for plugin updates
    checker = { enabled = true },
  }
})

require('config.lspconfig')
require('config.treesitter')
require('config.autopairs')
require('config.nvim-tree')
require('config.telescope')
require('config.lualine')
