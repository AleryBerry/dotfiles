return {
  "aleryberry/coq_nvim",
  branch = "custom-format",
  dependencies = {
    { "ms-jpq/coq.artifacts",  branch = "artifacts" },
    { 'ms-jpq/coq.thirdparty', branch = "3p" }
  },
  init = function()
    vim.g.coq_settings = {
      keymap = {
        jump_to_mark = "",
        pre_select = true
      },
      auto_start = "shut-up", -- if you want to start COQ at startup
      completion = {
        always = true,
        smart = true,
        skip_after = { "\t", "\n", " ", "(", ")", "[", "]", "{", "}" },
      },
      display = {
        preview = {
          border = {
            { "",  "NormalFloat" },
            { "",  "NormalFloat" },
            { "",  "NormalFloat" },
            { " ", "NormalFloat" },
            { "",  "NormalFloat" },
            { "",  "NormalFloat" },
            { "",  "NormalFloat" },
            { " ", "NormalFloat" }
          },
          enabled = true,
          positions = { north = 1, south = 2, west = 3, east = 4 }
        },
        statusline = {
          helo = false
        }
      },
      -- custom_format = function(item)
      --   return item
      -- end
    }
    require("coq_3p") {
      { src = "builtin/c" },
      { src = "builtin/clojure" },
      { src = "builtin/css" },
      { src = "builtin/haskell" },
      { src = "builtin/html" },
      { src = "builtin/js" },
      { src = "nvimlua",        short_name = "nLUA" },
      { src = "codeium",        short_name = "COD" },
      { src = "bc",             short_name = "MATH", precision = 6 },
      { src = "figlet",         short_name = "BIG" },
    }
  end,
  config = function()
    -- local capabilities = vim.lsp.protocol.make_client_capabilities()
    -- capabilities.workspace.didChangeWatchedFiles.dynamicRegistration = false
    -- local coq = require("coq")
    -- vim.lsp.config("*", coq.lsp_ensure_capabilities())
  end,
}
