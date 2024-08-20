local lspconfig = require 'lspconfig'
local capabilities = require('cmp_nvim_lsp').default_capabilities()
local luasnip = require 'luasnip'
local cmp = require 'cmp'


local on_attach = function(client, bufnr)
  require("better-diagnostic-virtual-text.api").setup_buf(bufnr, {
    inline = false,
    priority = 2003,
  })
  local bufopts = { noremap = true, silent = true, buffer = bufnr }
  vim.keymap.set('n', '<leader>r', vim.lsp.buf.rename, bufopts)
  vim.keymap.set('n', '<leader>c', require("actions-preview").code_actions, bufopts)
end

local handlers = {
  ['textDocument/typeDefinition'] = require 'lsputil.locations'.typeDefinition_handler,
  ['textDocument/implementation'] = require 'lsputil.locations'.implementation_handler,
  ['textDocument/documentSymbol'] = require 'lsputil.symbols'.document_handler,
}

require("actions-preview").setup {
  diff = {
    algorithm = "patience",
    ignore_whitespace = true,
  },
  telescope = require("telescope.themes").get_dropdown { winblend = 10 },
}

require("flutter-tools").setup {
  lsp = {
    on_attach = on_attach,
    handlers = handlers
  }
}


local servers = { 'pyright', 'rust_analyzer', 'tailwindcss',
  'tsserver', 'clangd', 'gdscript', 'lua_ls', 'java_language_server',
  'csharp_ls', 'zls', 'htmx', 'biome', 'cssls', 'superhtml', 'eslint', }

for _, lsp in pairs(servers) do
  lspconfig[lsp].setup {
    capabitilies = capabilities,
    on_attach = on_attach,
  }
end

cmp.setup({
  window = {
    -- completion = cmp.config.window.bordered(),
    -- documentation = cmp.config.window.bordered(),
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-k>'] = cmp.mapping.scroll_docs(-4),
    ['<C-j>'] = cmp.mapping.scroll_docs(4),
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = false,
    },
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        vim.fn.feedkeys(vim.api.nvim_replace_termcodes("<Plug>(Tabout)", true, true, true), "")
      end
    end, { 'i', 's' }),
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      else
        fallback()
      end
    end, { 'i', 's' }),
  }),
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
    { name = 'nvim_lua' },
    { name = 'buffer' },
    { name = 'path' }
  })
})

-- Set configuration for specific filetype.
cmp.setup.filetype('gitcommit', {
  sources = cmp.config.sources({
    { name = 'cmp_git' }, -- You can specify the `cmp_git` source if you were installed it.
  }, {
    { name = 'buffer' },
  })
})

-- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline({ '/', '?' }, {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = 'buffer' }
  }
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = 'path' }
  }, {
    { name = 'cmdline' }
  })
})

luasnip.filetype_extend("dart", { "flutter" })
