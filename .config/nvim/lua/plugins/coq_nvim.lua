local function inspect(value)
  vim.schedule(function()
    local lines = vim.split(vim.inspect(value), '\n')

    -- Create a modifiable buffer
    local buf = vim.api.nvim_create_buf(true, true)

    -- Create window with normal editing capabilitiese
    local win = vim.api.nvim_open_win(buf, true, {
      relative = 'cursor',
      width = math.min(80, vim.o.columns - 10),
      height = math.min(20, #lines),
      row = 1,
      col = 0,
      border = 'rounded',

      -- REMOVE the minimal style to enable full editing
      style = nil, -- This is key for enabling visual mode
    })


    -- Set buffer content and options
    vim.api.nvim_buf_set_lines(buf, 0, -1, true, lines)
    vim.api.nvim_buf_set_option(buf, 'filetype', 'lua')
    vim.api.nvim_buf_set_option(buf, 'modifiable', true)
    vim.api.nvim_buf_set_option(buf, 'readonly', false)


    -- Set window options for better editing experience
    vim.api.nvim_win_set_option(win, 'number', true)
    vim.api.nvim_win_set_option(win, 'relativenumber', false)
    vim.api.nvim_win_set_option(win, 'cursorline', true)
    vim.api.nvim_win_set_option(win, 'winhl', 'Normal:NormalFloat')

    -- Add keymaps for easier navigation
    vim.api.nvim_buf_set_keymap(buf, 'n', 'q', '<cmd>close!<CR>', { noremap = true, silent = true })
    vim.api.nvim_buf_set_keymap(buf, 'n', '<Esc>', '<cmd>close!<CR>', { noremap = true, silent = true })
    vim.api.nvim_buf_set_keymap(buf, 'v', '<Esc>', '<cmd>close!<CR>', { noremap = true, silent = true })

    -- Set focus to the new window
    vim.api.nvim_set_current_win(win)
  end)
end

return {
  "aleryberry/coq_nvim",
  branch = "custom-format",
  dependencies = {
    { "ms-jpq/coq.artifacts",  branch = "artifacts" },
    { 'ms-jpq/coq.thirdparty', branch = "3p" }
  },
  init = function()
    local kind_map = {
      -- Official Vim docs kinds
      ["function"] = "f",
      method = "f", -- Often grouped with function
      variable = "v",
      member = "m", -- For struct/class members
      field = "m",
      typedef = "t",
      define = "d",
      macro = "d",
      -- Add other common LSP kinds with sensible single-letter representations
      class = "C",
      struct = "S",
      interface = "I",
      module = "M",
      snippet = "s",
      text = "T",
      keyword = "K",
    }
    local default_kind = "?"
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
      custom_format = function(entry, item)
        local word, kind_desc = item.abbr:match("^(.*)%s+%[(.+)%]$")
        if not (word and kind_desc) then
          word = item.abbr
          kind_desc = (item.menu or ""):gsub("%s*「LS」%s*", "") -- try to get from menu
        end
        local clean_kind = kind_desc:gsub("%S+%s+", ""):lower()
        if not entry then return item end -- Safety check
        local highlights_info = require("colorful-menu").cmp_highlights(entry)

        -- If colorful-menu provided data, use it!
        if highlights_info then
          item.abbr = highlights_info.text
        end

        -- C. Set the 'kind' based on our new doc-compliant map
        local kind_name = (clean_kind or ""):lower()
        item.kind = kind_map[kind_name] or default_kind
        item.menu = entry.kind or ""
        item.abbr = word      -- Abbreviation is just the main word
        item.menu = kind_desc -- Menu shows the original, full description

        -- 4. Apply our custom highlight groups
        item.abbr_hlgroup = "PmenuAbbrCustom"
        item.kind_hlgroup = "PmenuKindCustom"

        return item
      end
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
    local coq = require("coq")                         -- add this
    vim.lsp.config("*", coq.lsp_ensure_capabilities()) -- after
  end,
}
