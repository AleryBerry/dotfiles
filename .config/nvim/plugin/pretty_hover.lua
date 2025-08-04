vim.pack.add({ "https://github.com/Fildo7525/pretty_hover" })

vim.keymap.set("n", "K", require('pretty_hover').hover)
