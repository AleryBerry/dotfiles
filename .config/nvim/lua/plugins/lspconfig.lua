vim.opt.completeopt = { "menu", "menuone", "noinsert", "popup" }

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
  vim.lsp.enable(lsp)
end
vim.lsp.config("lua_ls", {
  settings = {
    Lua = {
      diagnostics = {
        globals = { "vim" }
      }
    }
  }
})

local set_keymaps = function(client, bufnr)
  local bufopts = { noremap = true, silent = true, buffer = bufnr }
  if client.supports_method('textdocument/rename') then
    vim.keymap.set("n", "<leader>r", vim.lsp.buf.rename, bufopts)
  end

  if client.supports_method('textdocument/codeaction') then
    vim.keymap.set("n", "<leader>c", require("actions-preview").code_actions, bufopts)
  end
  if client.supports_method('textdocument/hover') then
    vim.keymap.set('n', 'K', require("pretty_hover").hover, bufopts)
  end

  if client.supports_method('textdocument/diagnostics') then
    vim.keymap.set('n', '<leader>di', vim.diagnostic.open_float)
    vim.keymap.set("n", "<leader>w", function()
      vim.diagnostic.jump({ count = vim.v.count1, float = false })
    end, bufopts)
    vim.keymap.set("n", "<leader>q", function()
      vim.diagnostic.jump({ count = -vim.v.count1, float = false })
    end, bufopts)
  end
  vim.keymap.set("n", "<leader>n", require("nvim-navbuddy").open, bufopts)
end

---@param keys string
local function feedkeys(keys)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(keys, true, false, true), 'n', true)
end

---Is the completion menu open?
local function pumvisible()
  return tonumber(vim.fn.pumvisible()) ~= 0
end

local set_completion = function(client, bufnr)
  local chars = {}; for i = 32, 126 do table.insert(chars, string.char(i)) end
  client.server_capabilities.completionProvider.triggerCharacters = chars
  vim.lsp.completion.enable(true, client.id, bufnr, {
    autotrigger = true,
    convert = function(item)
      return { abbr = item.label:gsub('%b()', '') }
    end,
  })
  vim.keymap.set('i', '<cr>', function()
    return pumvisible() and '<C-y>' or '<cr>'
  end, { expr = true })

  vim.keymap.set({ 'i', 's' }, '<Tab>', function()
    if pumvisible() then
      feedkeys '<C-n>'
    elseif vim.snippet.active { direction = 1 } then
      vim.snippet.jump(1)
    else
      feedkeys '<Tab>'
    end
  end, {})
  vim.keymap.set({ 'i', 's' }, '<S-Tab>', function()
    if pumvisible() then
      feedkeys '<C-p>'
    elseif vim.snippet.active { direction = -1 } then
      vim.snippet.jump(-1)
    else
      feedkeys '<S-Tab>'
    end
  end, {})
  vim.keymap.set('s', '<BS>', '<C-o>s', {})
end

vim.api.nvim_create_autocmd('LspAttach', {
  callback = function(args)
    local bufnr = args.buf
    local client = assert(vim.lsp.get_client_by_id(args.data.client_id))
    require('nvim-navbuddy').attach(client, bufnr)
    require("workspace-diagnostics").populate_workspace_diagnostics(client, bufnr)
    if client.supports_method('textdocument/completion') then
      set_completion(client, bufnr)
    end
    set_keymaps(client, bufnr)
    if not client:supports_method('textDocument/willSaveWaitUntil')
        and client:supports_method('textDocument/formatting') then
      vim.api.nvim_create_autocmd('BufWritePre', {
        buffer = bufnr,
        callback = function()
          vim.lsp.buf.format({ bufnr = args.buf, id = client.id, timeout_ms = 1000 })
        end
      })
    end
  end
})

return {
  'neovim/nvim-lspconfig',
  lazy = false,
  event = { 'BufReadPost', 'BufNewFile' },
  cmd = { 'LspInfo', 'LspInstall', 'LspUninstall' },
}
