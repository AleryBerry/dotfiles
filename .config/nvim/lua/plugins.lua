local function pconf(plugin)
  return "require(\"config." .. plugin .. "\")"
end

return require('packer').startup(function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  use {
    'windwp/nvim-autopairs',
    config = function() require("nvim-autopairs").setup {} end
  }
  -- Lazy loading:
  -- Load on specific commands
  use { 'tpope/vim-dispatch', opt = true, cmd = { 'Dispatch', 'Make', 'Focus', 'Start' } }

  -- Load on an autocommand event
  use { 'andymass/vim-matchup', event = 'VimEnter' }

  -- Post-install/update hook with neovim command
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

  -- Use specific branch, dependency and run lua file after load
  use({
    'glepnir/galaxyline.nvim',
    branch = 'main',
    -- your statusline
    config = pconf('galaxyline'),
    -- some optional icons
    requires = { 'nvim-tree/nvim-web-devicons', opt = true },
  })
  -- Use dependency and run lua function after load
  use { 'lewis6991/gitsigns.nvim', requires = { 'nvim-lua/plenary.nvim' },
    config = function() require('gitsigns').setup() end }

  use {
    'nvim-tree/nvim-tree.lua',
    requires = {
      'nvim-tree/nvim-web-devicons', -- optional, for file icons
    },
    use { 'neovim/nvim-lspconfig' },
    use { 'hrsh7th/cmp-nvim-lsp' },
    use { 'hrsh7th/cmp-buffer' },
    use { 'hrsh7th/cmp-path' },
    use { 'hrsh7th/cmp-cmdline' },
    use { 'hrsh7th/nvim-cmp' },
    use({ "L3MON4D3/LuaSnip", tag = "v<CurrentMajor>.*" }),
    use { 'hrsh7th/cmp-nvim-lsp-signature-help' },
    use { 'akinsho/flutter-tools.nvim' },
    use { 'nvim-lua/plenary.nvim' },
    use { 'akinsho/bufferline.nvim', tag = "v3.*", requires = 'nvim-tree/nvim-web-devicons' },
    use {
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
    use { 'echasnovski/mini.jump', branch = 'stable' },
    use { 'mg979/vim-visual-multi', branch = 'master' },
    use { 'lambdalisue/suda.vim' },
    use { 'tpope/vim-surround' },
    use { 'terryma/vim-expand-region' },
    use { 'michaelb/sniprun', run = 'bash ./install.sh',
      config = function() require('sniprun').setup({ display = { "NvimNotify" } }) end },
    use { 'rcarriga/nvim-notify' },
    use { 'mattn/emmet-vim' },
    use({ 'weilbith/nvim-code-action-menu', cmd = 'CodeActionMenu', }),
    use { 'kosayoda/nvim-lightbulb' },
    use {
      'm-demare/hlargs.nvim',
      requires = { 'nvim-treesitter/nvim-treesitter' }
    },
    use {
      'numToStr/Comment.nvim',
      config = function()
        require('Comment').setup()
      end
    },
    use { 'fedepujol/move.nvim' },
    use { "ahmedkhalf/project.nvim", config = function() require("project_nvim").setup {} end },
    use { 'nvim-telescope/telescope.nvim' },
    use { "startup-nvim/startup.nvim", requires = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
      config = function() require "startup".setup({ theme = "startify" }) end },
    use { "lukas-reineke/indent-blankline.nvim" },
    use { 'dstein64/nvim-scrollview' },
    use { 'romgrk/barbar.nvim', requires = 'nvim-web-devicons' },
    use { 'nvim-tree/nvim-web-devicons' },

    use({
      "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
      config = function()
        require("lsp_lines").setup()
      end,
    }),
    -- Themes
    use { 'KabbAmine/yowish.vim' },
    use { 'cocopon/iceberg.vim' },
    use { 'Mofiqul/dracula.nvim' },
    use { 'jacoborus/tender.vim' },
    use { 'sainnhe/gruvbox-material' },
    use { 'mrcjkb/haskell-tools.nvim' },
    use { 'nathom/filetype.nvim' },

    use { 'udalov/kotlin-vim' },
    use { 'mfussenegger/nvim-jdtls' },

    tag = 'nightly'
  }

  require('config.lspconfig')
  require('config.treesitter')
  require('config.autopairs')
  require('config.nvim-tree')
  require('config.telescope')
end)
