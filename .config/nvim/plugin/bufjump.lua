vim.pack.add({"https://github.com/kwkarlwang/bufjump.nvim"})

require('bufjump').setup({
	forward_key = "<leader>i",
	backward_key = "<leader>o"
})
