return {
  {
    'echasnovski/mini.ai',
    version = "*"
  },
  {
    'DrKJeff16/project.nvim',
    lazy = false,
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-telescope/telescope.nvim',
    },
    config = function() require('project_nvim').setup({}) end,
    cond = vim.fn.has('nvim-0.11') == 1,
  },
  {
    "cbochs/grapple.nvim",
    dependencies = {
      { "nvim-tree/nvim-web-devicons", lazy = true }
    },
    opts = {
      scope = "git", -- also try out "git_branch"
    },
    event = { "BufReadPost", "BufNewFile" },
    cmd = "Grapple",
    keys = {
      { "<leader>a", "<cmd>Grapple toggle<cr>",         desc = "Tag a file" },
      { "<c-n>",     "<cmd>Grapple toggle_tags<cr>",    desc = "Toggle tags menu" },
      { "<c-h>",     "<cmd>Grapple select index=1<cr>", desc = "Select first tag" },
      { "<c-j>",     "<cmd>Grapple select index=2<cr>", desc = "Select second tag" },
      { "<c-k>",     "<cmd>Grapple select index=3<cr>", desc = "Select third tag" },
      { "<c-l>",     "<cmd>Grapple select index=4<cr>", desc = "Select fourth tag" },
    }
  },
  { "echasnovski/mini.splitjoin",         version = false },
  {
    "Fildo7525/pretty_hover",
    event = "LspAttach",
    opts = {},
    init = function()
      vim.keymap.set("n", "K", require('pretty_hover').hover)
    end
  },
  { "artemave/workspace-diagnostics.nvim" },
  {
    'nvim-flutter/flutter-tools.nvim',
    lazy = false,
    dependencies = {
      'nvim-lua/plenary.nvim',
      'stevearc/dressing.nvim',
    },
    config = true,
    init = function() require("flutter-tools").setup({}) end
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
      },
      zindex = 20,
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
