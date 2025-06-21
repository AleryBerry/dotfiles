return {
  { 'echasnovski/mini.splitjoin',         version = false },
  { "Fildo7525/pretty_hover",             event = "LspAttach", opts = {} },
  { "artemave/workspace-diagnostics.nvim" },
  { "hinell/lsp-timeout.nvim", },
  {
    'nvim-flutter/flutter-tools.nvim',
    lazy = false,
    dependencies = {
      'nvim-lua/plenary.nvim',
      'stevearc/dressing.nvim',
    },
    config = true,
    init = function()
      require("flutter-tools").setup({})
    end
  },
  {
    "SmiteshP/nvim-navbuddy",
    dependencies = {
      "SmiteshP/nvim-navic",
      "MunifTanjim/nui.nvim"
    },
    opts = { lsp = { auto_attach = true } }
  },
  {
    "ray-x/lsp_signature.nvim",
    event = "InsertEnter",
    opts = {
      bind = true,
      floating_window_above_cur_line = true,
      always_trigger = true,
      transparency = 50,
      handler_opts = {
        border = "rounded"
      }
    },
  },
  {
    "romus204/referencer.nvim",
    config = function()
      require("referencer").setup()
    end
  },
  {
    "altermo/ultimate-autopair.nvim",
    event = { "InsertEnter", "CmdlineEnter" },
    branch = "v0.6", --recommended as each new version will have breaking changes
    init = function()
      require 'ultimate-autopair'.setup({})
    end
  },
  { "aznhe21/actions-preview.nvim", lazy = true },
  { "nvim-tree/nvim-web-devicons",  lazy = true },
  { "sainnhe/gruvbox-material",     lazy = true },
  {
    "mrcjkb/haskell-tools.nvim",
    version = '^6', -- Recommended
    lazy = false,   -- This plugin is already lazy
  },
  {
    'mrcjkb/rustaceanvim',
    version = '^6', -- Recommended
    lazy = false,   -- This plugin is already lazy
  },
  { "ggandor/leap.nvim" },
  { "ggandor/flit.nvim" },
  { "nvim-telescope/telescope-project.nvim" },
  { "lambdalisue/suda.vim" },
  {
    "kylechui/nvim-surround",
    version = "*", -- Use for stability; omit to use `main` branch for the latest features
    event = "VeryLazy",
    config = function() require("nvim-surround").setup({}) end
  },
  {
    "michaelb/sniprun",
    build = "bash ./install.sh",
    config = function() require("sniprun").setup({ display = { "NvimNotify" } }) end,
  },
  { "rcarriga/nvim-notify" },
  { "m-demare/hlargs.nvim" },
}
