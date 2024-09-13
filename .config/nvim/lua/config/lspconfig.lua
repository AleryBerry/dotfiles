local lspconfig = require("lspconfig")
local function capabilities()
  return vim.tbl_deep_extend(
    "force",
    vim.lsp.protocol.make_client_capabilities(),
    -- nvim-cmp supports additional completion capabilities, so broadcast that to servers.
    require("cmp_nvim_lsp").default_capabilities()
  )
end
local cmp = require("cmp")
local luasnip = require("luasnip")
luasnip.setup({})
require("luasnip.loaders.from_vscode").lazy_load({
  paths = { "../snippets/" },
})
local haskell_snippets = require("haskell-snippets").all
luasnip.add_snippets("haskell", haskell_snippets, { key = "haskell" })

local on_attach = function(client, bufnr)
  require("better-diagnostic-virtual-text.api").setup_buf(bufnr, {
    inline = false,
    priority = 2003,
  })
  local bufopts = { noremap = true, silent = true, buffer = bufnr }
  vim.keymap.set("n", "<leader>r", vim.lsp.buf.rename, bufopts)
  vim.keymap.set("n", "<leader>c", require("actions-preview").code_actions, bufopts)
end

require("actions-preview").setup({
  diff = {
    algorithm = "patience",
    ignore_whitespace = true,
  },
  telescope = require("telescope.themes").get_dropdown({ winblend = 10 }),
})

require("flutter-tools").setup({
  lsp = {
    on_attach = on_attach,
  },
})

local servers = {
  "pyright",
  "rust_analyzer",
  "tailwindcss",
  "ts_ls",
  "clangd",
  "gdscript",
  "lua_ls",
  "java_language_server",
  "csharp_ls",
  "zls",
  "htmx",
  "biome",
  "cssls",
  "superhtml",
  "eslint",
}

require("flutter-tools").setup({
  root_patterns = { ".git", "pubspec.yaml" }, -- patterns to find the root of your flutter project
  widget_guides = {
    enabled = false,
  },
  lsp = {
    on_attach = on_attach,
    capabilities = capabilities(), -- e.g. lsp_status capabilities
  },
})

for _, lsp in pairs(servers) do
  lspconfig[lsp].setup({
    on_attach = on_attach,
    capabilities = capabilities(),
  })
end

cmp.setup({
  window = {
    -- completion = cmp.config.window.bordered(),
    -- documentation = cmp.config.window.bordered(),
  },
  snippet = {
    -- REQUIRED - you must specify a snippet engine
    expand = function(args)
      luasnip.lsp_expand(args.body) -- For `luasnip` users.
      -- vim.snippet.expand(args.body)            -- For native neovim snippets (Neovim v0.10+)
    end,
  },
  mapping = cmp.mapping.preset.insert({
    ["<C-k>"] = cmp.mapping.scroll_docs(-4),
    ["<C-j>"] = cmp.mapping.scroll_docs(4),
    ["<C-e>"] = cmp.mapping.abort(),
    ["<CR>"] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Replace,
      select = false,
    }),

    ["<Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        vim.fn.feedkeys(vim.api.nvim_replace_termcodes("<Plug>(Tabout)", true, true, true), "")
      end
    end, { "i", "s" }),
    ["<S-Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      else
        fallback()
      end
    end, { "i", "s" }),
    ["<C-Space>"] = cmp.mapping(
      cmp.mapping.complete({
        reason = cmp.ContextReason.Auto,
      }),
      { "i", "c" }
    ),
  }),
  sources = cmp.config.sources({
    { name = "luasnip" },
    { name = "nvim_lsp" },
    { name = "buffer" },
    { name = "path" },
  }),
})

-- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline({ "/", "?" }, {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = "buffer" },
  },
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(":", {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = "path" },
  }, {
    { name = "cmdline" },
  }),
})

require("leap").opts.safe_labels = {}
require("leap").opts.preview_filter = function()
  return false
end
require("flit").setup({
  keys = { f = "f", F = "F", t = "t", T = "T" },
  -- A string like "nv", "nvo", "o", etc.
  labeled_modes = "vn",
  -- Repeat with the trigger key itself.
  clever_repeat = true,
  multiline = true,
  -- Like `leap`s similar argument (call-specific overrides).
  -- E.g.: opts = { equivalence_classes = {} }
  opts = {},
})
require("conform").setup({
  formatters_by_ft = {
    lua = { "stylua" },
    gdscript = { "gdformat" },
    -- Conform will run multiple formatters sequentially
    python = { "isort", "black" },
    -- You can customize some of the format options for the filetype (:help conform.format)
    rust = { "rustfmt", lsp_format = "fallback" },
    -- Conform will run the first available formatter
    javascript = { "prettierd", "prettier", stop_after_first = true },
  },
  format_on_save = {
    -- These options will be passed to conform.format()
    timeout_ms = 5000,
    lsp_format = "last",
  },
})

vim.g.haskell_tools = {
  hls = {
    on_attach = on_attach,
    capabilities = capabilities(),
  },
}
require("telescope").load_extension("ht")
