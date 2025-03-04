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
require("settings")

-- Setup lazy.nvim
require("lazy").setup {
  git = {
    -- defaults for the `Lazy log` command
    log = { "--since=2 days ago" }, -- show commits from the last 3 days
    timeout = 120,
    0,                              -- kill processes that take more than 2 minutes

    -- url_format = "rit@github.com:%s.git",
    -- lazy.nvim requires git >=2.19.0. If you really want to use lazy with an older version,
    -- then set the below to false. This should work, but is NOT supported and will
    -- increase downloads a lot.
    filter = true,
  },
  spec = {
    -- Lazy loading:
    -- Load on specific commands
    {
      "altermo/ultimate-autopair.nvim",
      event = { "InsertEnter", "CmdlineEnter" },
      branch = "v0.6", --recommended as each new version will have breaking changes
      opts = {
        --Config goes here
      },
    },
    { "eandrju/cellular-automaton.nvim" },
    {
      "mrcjkb/haskell-tools.nvim",
      version = "^4", -- Recommended
      lazy = false,   -- This plugin is already lazy
    },
    -- Load on an autocommand event
    { "andymass/vim-matchup",           event = "VimEnter" },
    {
      "j-hui/fidget.nvim",
      config = function()
        require("fidget").setup({
          progress = {
            poll_rate = 0,               -- How and when to poll for progress messages
            suppress_on_insert = true,   -- Suppress new messages while in insert mode
            ignore_done_already = true,  -- Ignore new tasks that are already complete
            ignore_empty_message = true, -- Ignore new tasks that don't contain a message
            -- Clear notification group when LSP server detaches
            clear_on_detach = function(client_id)
              local client = vim.lsp.get_client_by_id(client_id)
              return client and client.name or nil
            end,
            -- How to get a progress message's notification group key
            notification_group = function(msg)
              return msg.lsp_client.name
            end,
            ignore = {}, -- List of LSP servers to ignore

            -- Options related to how LSP progress messages are displayed as notifications
            display = {
              render_limit = 16, -- How many LSP messages to show at once
              done_ttl = 3, -- How long a message should persist after completion
              done_icon = "✔", -- Icon shown when all LSP progress tasks are complete
              done_style = "Constant", -- Highlight group for completed LSP tasks
              progress_ttl = math.huge, -- How long a message should persist when in progress
              -- Icon shown when LSP progress tasks are in progress
              progress_icon = { pattern = "dots", period = 1 },
              -- Highlight group for in-progress LSP tasks
              progress_style = "WarningMsg",
              group_style = "Title",   -- Highlight group for group name (LSP server name)
              icon_style = "Question", -- Highlight group for group icons
              priority = 30,           -- Ordering priority for LSP notification group
              skip_history = true,     -- Whether progress notifications should be omitted from history
              -- How to format a progress message
              format_message = require("fidget.progress.display").default_format_message,
              -- How to format a progress annotation
              format_annote = function(msg)
                return msg.title
              end,
              -- How to format a progress notification group's name
              format_group_name = function(group)
                return tostring(group)
              end,
              overrides = { -- Override options from the default notification config
                rust_analyzer = { name = "rust-analyzer" },
              },
            },

            -- Options related to Neovim's built-in LSP client
            lsp = {
              progress_ringbuf_size = 0, -- Configure the nvim's LSP progress ring buffer size
              log_handler = true,        -- Log `$/progress` handler invocations (for debugging)
            },

            -- Options related to notification subsystem
            notification = {
              poll_rate = 10,                -- How frequently to update and render notifications
              filter = vim.log.levels.ERROR, -- Minimum notifications level
              history_size = 128,            -- Number of removed messages to retain in history
              override_vim_notify = true,    -- Automatically override vim.notify() with Fidget
              -- Conditionally redirect notifications to another backend
              redirect = function(msg, level, opts)
                if opts and opts.on_open then
                  return require("fidget.integration.nvim-notify").delegate(msg, level, opts)
                end
              end,
            },

            -- Options related to integrating with other plugins
            integration = {
              ["nvim-tree"] = {
                enable = true, -- Integrate with nvim-tree/nvim-tree.lua (if installed)
              },
            },

            logger = {
              level = vim.log.levels.ERROR, -- Minimum logging level
              max_size = 10000,             -- Maximum log file size, in KB
              float_precision = 0.01,       -- Limit the number of decimals displayed for floats
              -- Where Fidget writes its logs to
              path = string.format("%s/fidget.nvim.log", vim.fn.stdpath("cache")),
            },
          }, -- options
        })
      end,
    },
    -- Post-install/update hook with neovim command
    {
      "nvim-treesitter/nvim-treesitter",
      build = ":TSUpdate",
      config = function()
        local configs = require("nvim-treesitter.configs")

        ---@diagnostic disable-next-line: missing-fields
        configs.setup({
          ensure_installed = {
            "c",
            "gdscript",
            "lua",
            "zig",
            "vim",
            "vimdoc",
            "query",
            "elixir",
            "heex",
            "javascript",
            "html",
          },
          sync_install = false,
          highlight = { enable = true },
          indent = { enable = true },
        })
      end,
    },
    {
      "mfussenegger/nvim-dap",
    },
    --  specific branch, dependency and run lua file after load
    {
      "nvim-lualine/lualine.nvim",
      dependencies = { "nvim-tree/nvim-web-devicons" },
    },
    {
      "kevinhwang91/nvim-ufo",
      dependencies = { "kevinhwang91/promise-async" },
    },
    {
      "folke/lazydev.nvim",
      ft = "lua", -- only load on lua files
      opts = {
        library = {
          -- See the configuration section for more details
          -- Load luvit types when the `vim.uv` word is found
          { path = "luvit-meta/library", words = { "vim%.uv" } },
        },
      },
    },
    { "Bilal2453/luvit-meta", lazy = true }, -- optional `vim.uv` typings
    {
      "sontungexpt/better-diagnostic-virtual-text",
      lazy = true,
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
            },
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
      "neovim/nvim-lspconfig",
      event = { "BufReadPost", "BufNewFile" },
      cmd = { "LspInfo", "LspInstall", "LspUninstall" },
    },
    {
      "hrsh7th/nvim-cmp",
      event = { "InsertEnter" },
    },
    { "hrsh7th/cmp-nvim-lsp" },
    { "hrsh7th/cmp-buffer" },
    { "hrsh7th/cmp-path" },
    { "hrsh7th/cmp-cmdline" },
    {
      "L3MON4D3/LuaSnip",
      -- follow latest release.
      version = "v2.*", -- Replace <CurrentMajor> by the latest released major (first number of latest release)
      -- install jsregexp (optional!).
      build = "make install_jsregexp",
      dependencies = {
        "rafamadriz/friendly-snippets",
        "Nash0x7E2/awesome-flutter-snippets",
      },
      config = function()
        require("luasnip/loaders/from_vscode").load({ paths = { "~/.local/share/nvim/lazy/friendly-snippets" } })
      end,
    },

    {
      "chrisgrieser/nvim-scissors",
      dependencies = { "nvim-telescope/telescope.nvim", "garymjr/nvim-snippets" },
      opts = {
        snippetDir = "./snippets/",
      },
    },
    { "saadparwaiz1/cmp_luasnip" },
    { "mrcjkb/haskell-snippets.nvim" },
    { "Nash0x7E2/awesome-flutter-snippets" },
    { "rafamadriz/friendly-snippets" },
    {
      "folke/twilight.nvim",
      opts = {
        -- your configuration comes here
        -- or leave it empty to use the default settings
        -- refer to the configuration section below
      },
    },
    { "ggandor/leap.nvim" },
    { "ggandor/flit.nvim" },
    {
      "stevearc/conform.nvim",
      opts = {},
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
    {
      "stevearc/aerial.nvim",
      opts = {},
      -- Optional dependencies
      dependencies = {
        "nvim-treesitter/nvim-treesitter",
        "nvim-tree/nvim-web-devicons",
      },
    },
    {
      "ray-x/lsp_signature.nvim",
      event = "VeryLazy",
      opts = {},
      config = function(_, opts)
        require("lsp_signature").setup(opts)
      end,
    },
    {
      "akinsho/flutter-tools.nvim",
      lazy = false,
      dependencies = {
        "nvim-lua/plenary.nvim",
        "stevearc/dressing.nvim", -- optional for vim.ui.select
      },
      config = false,
    },
    { "nvim-lua/plenary.nvim" },
    { "akinsho/bufferline.nvim", version = "*", dependencies = "nvim-tree/nvim-web-devicons" },
    {
      "abecodes/tabout.nvim",
      event = 'InsertEnter',
      config = function(_, opts)
        require('tabout').setup(opts)
      end,
      opts = {
        tabkey = "<Tab>",             -- key to trigger tabout, set to an empty string to disable
        backwards_tabkey = "<S-Tab>", -- key to trigger backwards tabout, set to an empty string to disable
        act_as_tab = false,           -- shift content if tab out is not possible
        act_as_shift_tab = false,     -- reverse shift content if tab out is not possible (if your keyboard/terminal supports <S-Tab>)
        default_tab = "<C-t>",        -- shift default action (only at the beginning of a line, otherwise <TAB> is used)
        default_shift_tab = "<C-d>",  -- reverse shift default action,
        enable_backwards = true,      -- well ...
        completion = false,           -- if the tabkey is used in a completion pum
        tabouts = {
          { open = "'", close = "'" },
          { open = '"', close = '"' },
          { open = "`", close = "`" },
          { open = "(", close = ")" },
          { open = "[", close = "]" },
          { open = "{", close = "}" },
          { open = "<", close = ">" },
        },
        ignore_beginning = true, --[[ if the cursor is at the beginning of a filled element it will rather tab out than shift the content ]]
        exclude = {},                                   -- tabout will ignore these filetypes
      },
      dependencies = { "nvim-treesitter", "nvim-cmp" }, -- or require if not used so far
    },
    { "mg979/vim-visual-multi", branch = "master" },
    { "lambdalisue/suda.vim" },
    {
      "kylechui/nvim-surround",
      version = "*", -- Use for stability; omit to use `main` branch for the latest features
      event = "VeryLazy",
      config = function()
        require("nvim-surround").setup({
          -- Configuration here, or leave empty to use defaults
        })
      end
    },
    { "terryma/vim-expand-region" },
    {
      "michaelb/sniprun",
      build = "bash ./install.sh",
      config = function()
        require("sniprun").setup({ display = { "NvimNotify" } })
      end,
    },
    { "rcarriga/nvim-notify" },
    { "aznhe21/actions-preview.nvim" },
    { "mattn/emmet-vim" },
    { "kosayoda/nvim-lightbulb" },
    {
      "m-demare/hlargs.nvim",
      dependencies = { "nvim-treesitter/nvim-treesitter" },
    },
    {
      "numToStr/Comment.nvim",
      opts = {},
    },
    {
      "fedepujol/move.nvim",
      opts = {
        char = {
          enable = true
        }
      },
    },
    {
      "LennyPhoenix/project.nvim",
      branch = "fix-get_clients",
      config = function()
        require("project_nvim").setup({})
      end,
    },
    {
      "aleryberry/hlchunk.nvim",
      event = { "BufReadPre", "BufNewFile" },
      config = function()
        require("hlchunk").setup({})
      end
    },
    { "nvim-telescope/telescope.nvim" },
    { "HiPhish/rainbow-delimiters.nvim" },
    { "dstein64/nvim-scrollview" },
    { "romgrk/barbar.nvim",             dependencies = "nvim-web-devicons" },
    { "nvim-tree/nvim-web-devicons" },
    -- { "RishabhRD/nvim-lsputils" },
    -- { "RishabhRD/popfix" },

    -- Themes
    { "KabbAmine/yowish.vim",           lazy = true },
    {
      "xero/miasma.nvim",
      lazy = false,
      priority = 1000,
      config = function()
        vim.cmd("colorscheme miasma")
      end,
    },
    {
      "luukvbaal/statuscol.nvim",
      config = function()
        local builtin = require("statuscol.builtin")
        require("statuscol").setup({
          segments = {
            { text = { "%s" },             click = "v:lua.ScSa" },
            { text = { builtin.lnumfunc }, click = "v:lua.ScLa", },
            {
              text = { " ", builtin.foldfunc, " " },
              condition = { builtin.not_empty, true, builtin.not_empty },
              click = "v:lua.ScFa"
            },
          }
        })
      end,
    },
    { "cocopon/iceberg.vim",         lazy = true },
    { "Mofiqul/dracula.nvim",        lazy = true },
    { "jacoborus/tender.vim",        lazy = true },
    { "f4z3r/gruvbox-material.nvim", lazy = true },
    { "PinpongTp/comic",             lazy = true },
    { "udalov/kotlin-vim",           lazy = true },
    { "mfussenegger/nvim-jdtls",     lazy = true },
  },
  -- Configure any other settings here. See the documentation for more details.
  -- colorscheme that will be used when installing plugins.
  install = { colorscheme = { "habamax" } },
  -- automatically check for plugin updates
  checker = { enabled = true },
}
require("nvim-lightbulb").setup({
  priority = 10,
  hide_in_unfocused_buffer = true,
  validate_config = "auto",
  link_highlights = true,
  code_lenses = false,
  float = {
    enabled = false,
  },
  line = {
    enabled = false,
  },
  ignore = {
    actions_without_kind = false,
  },
  autocmd = {
    enabled = true,
  },
  status_text = {
    enabled = false,
  },
  sign = {
    enabled = false,
    -- Text to show in the sign column.
    -- Must be between 1-2 characters.
    text = "💡",
    -- Highlight group to highlight the sign column text.
    hl = "LightBulbSign",
  },
  virtual_text = {
    enabled = true,
    -- Text to show in the virt_text.
    text = "💡",
    -- Position of virtual text given to |nvim_buf_set_extmark|.
    -- Can be a number representing a fixed column (see `virt_text_pos`).
    -- Can be a string representing a position (see `virt_text_win_col`).
    pos = "eol",
    -- Highlight group to highlight the virtual text.
    hl = "LightBulbVirtualText",
    -- How to combine other highlights with text highlight.
    -- See `hl_mode` of |nvim_buf_set_extmark|.
    hl_mode = "combine",
  },
  number = {
    enabled = true,
    -- Highlight group to highlight the number column if there is a lightbulb.
    hl = "LightBulbNumber",
  },
})
vim.cmd("colorscheme miasma")

local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config['html'] = {
  install_info = {
    url = "~/Git/superhtml/tree-sitter-superhtml", -- local path or git repo
    files = { "src/parser.c" },                    -- note that some parsers also require src/scanner.c or src/scanner.cc
  },
  filetype = "html",                               -- if filetype does not match the parser name
}
vim.treesitter.language.register("superhtml", "html")

local handler = function(virtText, lnum, endLnum, width, truncate)
  local newVirtText = {}
  local suffix = (' 󰁂 %d '):format(endLnum - lnum)
  local sufWidth = vim.fn.strdisplaywidth(suffix)
  local targetWidth = width - sufWidth
  local curWidth = 0
  for _, chunk in ipairs(virtText) do
    local chunkText = chunk[1]
    local chunkWidth = vim.fn.strdisplaywidth(chunkText)
    if targetWidth > curWidth + chunkWidth then
      table.insert(newVirtText, chunk)
    else
      chunkText = truncate(chunkText, targetWidth - curWidth)
      local hlGroup = chunk[2]
      table.insert(newVirtText, { chunkText, hlGroup })
      chunkWidth = vim.fn.strdisplaywidth(chunkText)
      -- str width returned from truncate() may less than 2nd argument, need padding
      if curWidth + chunkWidth < targetWidth then
        suffix = suffix .. (' '):rep(targetWidth - curWidth - chunkWidth)
      end
      break
    end
    curWidth = curWidth + chunkWidth
  end
  table.insert(newVirtText, { suffix, 'MoreMsg' })
  return newVirtText
end
require('ufo').setup({
  ---@diagnostic disable-next-line: unused-local
  provider_selector = function(bufnr, filetype, buftype)
    return { 'treesitter', 'indent' }
  end,
  fold_virt_text_handler = handler,
})

vim.keymap.set('n', 'zR', require('ufo').openAllFolds)
vim.keymap.set('n', 'zM', require('ufo').closeAllFolds)

require('hlchunk').setup({
  chunk = {
    enable = true,
    use_treesitter = true,
    textobject = "ic"
  },
  indent = {
    enable = true
  }
})

require("config.lspconfig")
require("config.treesitter")
require("config.telescope")
require("config.lualine")
require("config.flutter-tools")
