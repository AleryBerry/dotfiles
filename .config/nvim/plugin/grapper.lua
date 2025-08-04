vim.pack.add({
  "https://github.com/cbochs/grapple.nvim",
  "https://github.com/nvim-tree/nvim-web-devicons" 
})

require('grapple').setup({
  scope = "git", -- also try out "git_branch"
  icons = false, -- setting to "true" requires "nvim-web-devicons"
  status = false,
})

vim.keymap.set("n", "<leader>a", "<cmd>Grapple toggle<cr>", {desc = "Tag a file", silent = true})
vim.keymap.set("n", "<c-n>", "<cmd>Grapple toggle_tags<cr>", {silent = true, desc = "Toggle tags menu"} )
vim.keymap.set("n", "<c-h>", "<cmd>Grapple select index=1<cr>", {silent = true, desc = "Select first tag"} )
vim.keymap.set("n", "<c-j>", "<cmd>Grapple select index=2<cr>", {silent = true, desc = "Select second tag"} )
vim.keymap.set("n", "<c-k>", "<cmd>Grapple select index=3<cr>", {silent = true, desc = "Select third tag"} )
vim.keymap.set("n", "<c-l>", "<cmd>Grapple select index=4<cr>", {silent = true, desc = "Select fourth tag"} )
