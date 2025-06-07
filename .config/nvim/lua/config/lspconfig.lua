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

---@diagnostic disable-next-line: unused-local
local on_attach = function(client, bufnr)
  require("better-diagnostic-virtual-text.api").setup_buf(bufnr, {
    inline = false,
    priority = 2003,
    ui = {
      wrap_line_after = true, -- wrap the line after this length to avoid the virtual text is too long
      left_kept_space = 3,    --- the number of spaces kept on the left side of the virtual text, make sure it enough to custom for each line
      right_kept_space = 3,   --- the number of spaces kept on the right side of the virtual text, make sure it enough to custom for each line
      arrow = "  ",
      up_arrow = "  ",
      down_arrow = "  ",
      above = true, -- the virtual text will be displayed above the line
    },
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
    capabilities = capabilities()
  },
})

lspconfig["rust_analyzer"].setup({
  on_attach = on_attach,
  capabilities = capabilities(),
  settings = {
    ['rust_analyzer'] = {
      cargo = {
        allFeatures = true,
      },
      checkOnSave = {
        command = 'clippy',
      },
      inlayHints = { locationLinks = false },
      diagnostics = {
        enable = true,
        experimental = {
          enable = true,
        },
      },
    },

  },
})

local servers = {
  "pyright",
  "tailwindcss",
  "ts_ls",
  "clangd",
  "gdscript",
  "nim_langserver",
  "lua_ls",
  "java_language_server",
  "csharp_ls",
  "sourcekit",
  "gleam",
  "zls",
  "gopls",
  "biome",
  "cssls",
  "superhtml",
  "eslint",
}

for _, lsp in pairs(servers) do
  lspconfig[lsp].setup({
    on_attach = on_attach,
    capabilities = capabilities(),
  })
end


lspconfig.htmx.setup({
  cmd = { "htmx-lsp2" },
  on_attach = on_attach,
  capabilities = capabilities(),
})

cmp.setup({
  window = {
    -- completion = cmp.config.window.bordered(),
    -- documentation = cmp.config.window.bordered(),
  },
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body) -- For `luasnip` users.
      -- vim.snippet.expand(args.body)            -- For native neovim snippets (Neovim v0.10+)
    end,
  },
  mapping = {
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
      else
        vim.fn.feedkeys(vim.api.nvim_replace_termcodes("<Plug>(TaboutMulti)", true, true, true), "")
      end
    end, { "i", "s" }),
    ["<S-Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      else
        vim.fn.feedkeys(vim.api.nvim_replace_termcodes("<Plug>(TaboutBackMulti)", true, true, true), "")
      end
    end, { "i", "s" }),
    ["<C-Space>"] = cmp.mapping(function(fallback)
      cmp.complete({
        reason = cmp.ContextReason.Auto,
      })
    end, { "i", "s" }),
  },
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
    python = { "isort", "black" },
    rust = { "rustfmt", lsp_format = "fallback" },
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


vim.lsp.handlers['textDocument/publishDiagnostics'] = function(err, result, ctx)
  local bufnr = vim.uri_to_bufnr(result.uri)
  if not bufnr then
    return
  end
  for key, diag in pairs(result.diagnostics) do
    local e = diag
    if diag.message ~= nil then
      local message, _ = string.gsub(e.message, "\n+", ". ")
      message = string.gsub(message, "%.%.", ". ")
      message = string.gsub(message, "%s%s", " ")
      e.message = message
    end
    result.diagnostics[key] = e
  end
  vim.lsp.diagnostic.on_publish_diagnostics(err, result, ctx)
end
