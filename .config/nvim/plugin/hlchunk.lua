vim.pack.add({
	"https://github.com/shellRaining/hlchunk.nvim"
})

require("hlchunk").setup({
	chunk = {
		enable = true,
		use_treesitter = true,
		textobject = "ic"
	},
	indent = {
		enable = true
	}
})
