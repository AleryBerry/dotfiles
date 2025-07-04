return {
  "aleryberry/coq_nvim",
  dependencies = {
    { "ms-jpq/coq.artifacts",  branch = "artifacts" },
    { 'ms-jpq/coq.thirdparty', branch = "3p" }
  },
  init = function()
    vim.g.coq_settings = {
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
    }
    vim.api.nvim_create_autocmd("ColorScheme", {
      pattern = "*", -- Run for any colorscheme
      callback = function()
        vim.api.nvim_set_hl(0, 'PmenuAbbrCustom', { fg = '#FFC8C6' })
        vim.api.nvim_set_hl(0, 'PmenuKindCustom', { fg = '#000000' })
        -- Main completion menu
        vim.api.nvim_set_hl(0, 'Pmenu', { bg = '#1e222a', fg = '#abb2bf' })    -- Normal item
        vim.api.nvim_set_hl(0, 'PmenuSel', { bg = '#3e4452', fg = '#ffffff' }) -- Selected item
        vim.api.nvim_set_hl(0, 'PmenuSbar', { bg = '#1e222a' })                -- Scrollbar background
        vim.api.nvim_set_hl(0, 'PmenuThumb', { bg = '#528bff' })               -- Scrollbar thumb

        -- Documentation window (preview)
        vim.api.nvim_set_hl(0, 'PmenuDoc', { bg = '#1e222a', fg = '#abb2bf' }) -- Documentation bg
        vim.api.nvim_set_hl(0, 'PmenuDocBorder', { fg = '#61afef' })
        vim.cmd([[
  augroup CompletionColors
    autocmd!
    autocmd VimEnter,ColorScheme * hi! link PmenuSel Normal
    autocmd VimEnter,ColorScheme * hi! link Pmenu NormalFloat
  augroup END
]])
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
    local coq = require("coq")                        
    vim.lsp.config("*", coq.lsp_ensure_capabilities()) 
  end,
}
