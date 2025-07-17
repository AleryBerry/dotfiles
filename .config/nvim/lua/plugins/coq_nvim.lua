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
        jump_to_mark = ""
      },
      auto_start = true, -- if you want to start COQ at startup
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
      custom_format = function(item)
        return item
      end
    }
    vim.api.nvim_create_autocmd("ColorScheme", {
      pattern = "*", -- Run for any colorscheme
      callback = function()
        vim.api.nvim_set_hl(0, 'PmenuAbbrCustom', { fg = '#FFC8C6' })
        vim.api.nvim_set_hl(0, 'PmenuKindCustom', { fg = '#000000' })
      end, -- The function to run
    })
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
    local coq = require("coq")                         -- add this
    vim.lsp.config("*", coq.lsp_ensure_capabilities()) -- after
  end,
}
