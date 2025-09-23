require("yanky").setup({})

vim.keymap.set({ "n", "x" }, "p", function() require('yanky').put("p", false) end)
vim.keymap.set({ "n", "x" }, "P", function() require('yanky').put("P", false) end)

vim.keymap.set("n", "<c-p>", function() require('yanky').cycle(1) end)
vim.keymap.set("n", "<c-[>", function() require('yanky').cycle(-1) end)

