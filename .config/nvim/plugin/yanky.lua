require("yanky").setup({
  ring = {
    history_length = 100,
    sync_with_numbered_registers = true,
    cancel_event = "update",
    ignore_registers = { "_" },
    update_register_on_cycle = false,
    permanent_wrapper = nil,
  },
  picker = {
    select = {
      action = nil, -- nil to use default put action
    },
  },
  highlight = {
    on_put = true,
    on_yank = true,
    timer = 500,
  },
  preserve_cursor_position = {
    enabled = true,
  },
  textobj = {
    enabled = false,
  },
}
)

vim.keymap.set({ "n", "x" }, "p", "<Plug>(YankyPutAfter)", { silent= true})
vim.keymap.set({ "n", "x" }, "P", "<Plug>(YankyPutBefore)", { silent= true})

vim.keymap.set("n", "<c-p>", "<Plug>(YankyPreviousEntry)")
vim.keymap.set("n", "<c-m>", "<Plug>(YankyNextEntry)")
