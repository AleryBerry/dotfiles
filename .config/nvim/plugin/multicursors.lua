vim.pack.add({
	"https://github.com/smoka7/multicursors.nvim",
	"https://github.com/nvimtools/hydra.nvim"
})

vim.keymap.set({ "v", "n" }, "<C-g>", "<cmd>MCstart<CR>", { silent = true })
require("multicursors").setup({
	DEBUG_MODE = false,
	create_commands = true,
	updatetime = 50,
	nowait = true,
	mode_keys = {
		append = 'a',
		change = 'c',
		extend = 'e',
		insert = 'i',
	},
	normal_keys = normal_keys,
	insert_keys = insert_keys,
	extend_keys = extend_keys,
	-- hint_config = false,
	hint_config = {
		float_opts = {
			border = 'rounded',
		},
		position = 'bottom',
	},
	-- accepted values:
	-- -1 true: generate hints
	-- -2 false: don't generate hints
	-- -3 [[multi line string]] provide your own hints
	-- -4 fun(heads: Head[]): string - provide your own hints
	generate_hints = {
		normal = true,
		insert = true,
		extend = true,
		config = {
			column_count = 2,
			max_hint_length = 25,
		}
	},
})
