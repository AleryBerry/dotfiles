require('nvim-treesitter').setup({
	ensure_installed = {},
	auto_install = true,
	highlight = {
		enable = true,
		-- Or use a function for more flexibility, e.g. to disable slow treesitter highlight for large files
		disable = function(_, buf)
			local max_filesize = 100 * 1024 -- 100 KB
			local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
			if ok and stats and stats.size > max_filesize then
				return true
			end
		end,
		additional_vim_regex_highlighting = false,
	},
	indent = {
		enable = true
	},
})
-- configuration
require("nvim-treesitter-textobjects").setup({
	select = {
		lookahead = true,
		move = {
			-- whether to set jumps in the jumplist
			set_jumps = true,
		},
		include_surrounding_whitespace = false,
	},
})

-- keymaps
-- You can use the capture groups defined in `textobjects.scm`
vim.keymap.set({ "x", "o" }, "af", function()
	require ("nvim-treesitter-textobjects.select").select_textobject("@function.outer", "textobjects")
end)
vim.keymap.set({ "x", "o" }, "if", function()
	require ("nvim-treesitter-textobjects.select").select_textobject("@function.inner", "textobjects")
end)
vim.keymap.set({ "x", "o" }, "ac", function()
	require ("nvim-treesitter-textobjects.select").select_textobject("@class.outer", "textobjects")
end)
vim.keymap.set({ "x", "o" }, "ic", function()
	require ("nvim-treesitter-textobjects.select").select_textobject("@class.inner", "textobjects")
end)
-- You can also use captures from other query groups like `locals.scm`
vim.keymap.set({ "x", "o" }, "as", function()
	require ("nvim-treesitter-textobjects.select").select_textobject("@local.scope", "locals")
end)
-- keymaps
vim.keymap.set("n", "<leader>Q", function()
	require("nvim-treesitter-textobjects.swap").swap_next "@parameter.inner"
end)
vim.keymap.set("n", "<leader>W", function()
	require("nvim-treesitter-textobjects.swap").swap_previous "@parameter.outer"
end)
-- configuration
require("nvim-treesitter-textobjects").setup {
}

local move = require("nvim-treesitter-textobjects.move")
vim.keymap.set({ "n", "x", "o" }, "]]", function()
	require("nvim-treesitter-textobjects.move").goto_next_start("@class.outer", "textobjects")
end)
vim.keymap.set({ "n", "x", "o" }, "]o", function()
	move.goto_next_start({ "@loop.inner", "@loop.outer" }, "textobjects")
end)

vim.keymap.set({ "n", "x", "o" }, "]f", function()
	require("nvim-treesitter-textobjects.move").goto_next_start({"@function.outer", }, "textobjects")
end)
vim.keymap.set({ "n", "x", "o" }, "]F", function()
	require("nvim-treesitter-textobjects.move").goto_next_end({"@function.outer", }, "textobjects")
end)
vim.keymap.set({ "n", "x", "o" }, "[f", function()
	require("nvim-treesitter-textobjects.move").goto_previous_start({"@function.outer", }, "textobjects")
end)

vim.keymap.set({ "n", "x", "o" }, "[c", function()
	require("nvim-treesitter-textobjects.move").goto_previous_start("@class.outer", "textobjects")
end)

vim.keymap.set({ "n", "x", "o" }, "]i", function()
	require("nvim-treesitter-textobjects.move").goto_next("@conditional.outer", "textobjects")
end)
vim.keymap.set({ "n", "x", "o" }, "[i", function()
	require("nvim-treesitter-textobjects.move").goto_previous("@conditional.outer", "textobjects")
end)

