vim.pack.add({ "https://github.com/folke/snacks.nvim" })

vim.keymap.set("n", "<leader>p", Snacks.picker.projects, {silent = true})
vim.keymap.set("n", "<leader>f", Snacks.picker.files, {silent = true})

